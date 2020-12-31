open Base
open Stdio

module Latex_expand = struct
  Bibtex2html_lib.Latexmacros.html_entities ()

  let macros s =
    let fd_in, fd_out = Unix.pipe () in
    let in_ch = Unix.in_channel_of_descr fd_in
    and out_ch = Unix.out_channel_of_descr fd_out in
    Bibtex2html_lib.Latexmacros.out_channel := out_ch;
    Bibtex2html_lib.Latexscan.main (Lexing.from_string s);
    Out_channel.close out_ch;
    let res = In_channel.input_all in_ch in
    In_channel.close in_ch;
    res

  let accents = Bibtex2html_lib.Latex_accents.normalize false

  let all s = accents s |> macros

  let omap ?(f = fun x -> x) o =
    Option.value_map ~default:"" ~f:(fun x -> all x |> f) o
end

module Author = struct
  type t = {firstnames: string list; lastname: string}

  let parse field =
    let parse_author s =
      match String.lsplit2 ~on:',' s with
      | None ->
          {firstnames= []; lastname= s}
      | Some (last, first) ->
          let first =
            String.split_on_chars ~on:[' '; '.'] first
            |> List.filter ~f:(fun s -> String.length s > 0)
          in
          {firstnames= first; lastname= last}
    in
    Str.split (Str.regexp "[ \t\n]+and[ \t\n]+") field
    |> List.map ~f:parse_author

  let list_to_html l =
    let format_author a =
      let initials =
        List.map
          ~f:(fun s ->
            let s = Latex_expand.accents s in
            (* Initial with rule for hyphen name. Takes HTML accents into account. *)
            Str.(global_replace (regexp {|\(-?\(&[A-Za-z]*;\|[^-&]\)\)[^-]*|}) {|\1.|} s) ^ " ")
          a.firstnames
        |> String.concat
      in
      initials ^ (Latex_expand.accents a.lastname)
    in
    let rec format_authors_rec acc oxford l =
      match l, oxford with
      | [], _ ->
          acc
      | [h], true ->
          acc ^ ", and " ^ format_author h
      | [h], false ->
          acc ^ " and " ^ format_author h
      | h :: t, _ ->
          format_authors_rec (acc ^ ", " ^ format_author h) true t
    in
    match l with
    | [] -> ""
    | h :: t -> format_authors_rec (format_author h) false t

  let firstnames a = a.firstnames
  let lastname a = a.lastname
end

module Publication = struct
  type article =
    { journal: string option
    ; volume: string option
    ; number: string option
    ; year: string option
    ; pages: string option }
  type inproceedings =
    { booktitle: string option
    ; volume: string option
    ; series: string option
    ; year: string option
    ; publisher: string option
    ; pages: string option }

  type t =
    | Article of article
    | Inproceedings of inproceedings

  let format_pages pages = (* TODO: parse pages *)
        ": " ^ pages |> Str.(global_replace (regexp "-+") "–")

  let to_html p =
    let omap = Latex_expand.omap in
    match p with
    | Article x ->
      omap x.journal
      ^ omap ~f:(( ^ ) " ") x.volume
      ^ omap ~f:(( ^ ) ", no.&nbsp;") x.number
      ^ omap ~f:(fun year -> " (" ^ year ^ ")") x.year
      ^ omap ~f:format_pages x.pages
    | Inproceedings x ->
      omap ~f:(fun bt -> bt ^ ".") x.booktitle
      ^ omap ~f:(( ^ ) " ") x.publisher
      ^ omap ~f:(( ^ ) " ") x.volume
      ^ omap ~f:format_pages x.pages
      ^ omap ~f:(fun year -> ", " ^ year) x.year
end

module Bibentry = struct
  type t =
    { publication: Publication.t
    ; key: string
    ; title: string
    ; author: Author.t list
    ; url: string option
    ; arxiv: string option }

  let otag tag f =
    match f with None -> "" | Some s -> "\t" ^ tag ^ " = {" ^ s ^ "}"

  let publication_type = function
    | Publication.Article _ -> "article"
    | Publication.Inproceedings _ -> "inproceedings"

  let publication_tags p =
    let tags = match p with
      | Publication.Article e ->
        [ (otag "journal" e.journal)
        ; (otag "volume" e.volume)
        ; (otag "number" e.number)
        ; (otag "year" e.year)
        ; (otag "pages" e.pages) ]
      | Publication.Inproceedings e ->
        [ (otag "booktitle" e.booktitle)
        ; (otag "volume" e.volume)
        ; (otag "series" e.series)
        ; (otag "year" e.year)
        ; (otag "publisher" e.publisher)
        ; (otag "pages" e.pages) ]
    in 
    List.filter tags ~f:(fun s -> not (String.is_empty s))

  let to_bib e =
    let rec author_to_bib acc =
      let firstnames, lastname = Author.(firstnames, lastname) in
      function
      | [] -> acc
      | [hd] ->
          acc ^ lastname hd ^ ", " ^ String.concat ~sep:" " (firstnames hd)
      | hd :: tl ->
          author_to_bib
            ( acc
            ^ lastname hd ^ ", " ^ String.concat ~sep:" " (firstnames hd)
            ^ " and " )
            tl
    in
    let all_tags = List.concat
        [ [ ("\ttitle = {" ^ e.title ^ "}")
          ; ("\tauthor = {" ^ (author_to_bib "" e.author) ^ "}") ]
        ; publication_tags e.publication
        ; [ (otag "url" e.url)
          ; (otag "arxiv" e.arxiv) ] ]
    in
    "@" ^ (publication_type e.publication) ^ "{" ^ e.key ^ ",\n"
    ^ String.concat ~sep:",\n"
      (List.filter ~f:(fun s -> not (String.is_empty s)) all_tags)
    ^ "\n}\n"
end

module Bibdb = struct
  module Bibtex = Bibtex2html_lib.Bibtex
  module Readbib = Bibtex2html_lib.Readbib

  type t = Bibtex.biblio

  let of_files =
    List.fold ~init:Bibtex.empty_biblio
      ~f:(fun b fp ->
        Bibtex.merge_biblios b (Readbib.read_entries_from_file fp))

  let find_entry db key =
    let string_of_atoms ?prepend:(p = "") ?append:(ap = "") atoms =
      List.fold
        ~f:(fun s a -> s ^ match a with Bibtex.Id a -> a | Bibtex.String a -> a)
        ~init:p atoms
      ^ ap
    in
    let prop_get ?(fail = false) properties tag =
      let missing_field_errmsg =
        tag ^ " field not found or empty in item " ^ key
      in
      match List.Assoc.find ~equal:String.equal properties tag with
      | None ->
          if fail then failwith missing_field_errmsg else None
      | Some f -> (
        match string_of_atoms f with
        | "" ->
            if fail then failwith missing_field_errmsg else None
        | s -> Some s )
    in
    let get_publication_data entrytype properties =
      match entrytype with
      | "inproceedings" -> Publication.Inproceedings
        { booktitle= prop_get properties "booktitle"
        ; volume= prop_get properties "volume"
        ; series= prop_get properties "series"
        ; year= prop_get properties "year"
        ; publisher= prop_get properties "publisher"
        ; pages= prop_get properties "pages" }
      | "article" -> Publication.Article
        { journal= prop_get properties "journal"
        ; volume= prop_get properties "volume"
        ; number= prop_get properties "number"
        ; year= prop_get properties "year"
        ; pages= prop_get properties "pages" }
      | x -> failwith ("Publication type " ^ x ^ "not implemented")
    in
    match Bibtex.find_entry key db with
    | exception Caml.Not_found ->
        None
    | item -> (
      match item with
      | Comment _ | Preamble _ | Abbrev (_, _) ->
          None
      | Entry (entrytype, key, properties) ->
          Some (
            { publication= get_publication_data entrytype properties
            ; key
            ; title=
                prop_get properties ~fail:true "title"
                |> (Option.value_exn : string option -> string)
            ; author=
                prop_get properties ~fail:true "author"
                |> (Option.value_exn : string option -> string) 
                |> Author.parse
            ; url= prop_get properties "url"
            ; arxiv= prop_get properties "arxiv" } : Bibentry.t) )

end
