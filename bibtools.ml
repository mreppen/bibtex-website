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
    let rec format_authors_rec acc l =
      match l with
      | [] ->
          acc
      | [h] ->
          acc ^ ", and " ^ format_author h
      | h :: t ->
          format_authors_rec (acc ^ ", " ^ format_author h) t
    in
    match l with [] -> "" | h :: t -> format_authors_rec (format_author h) t

  let firstnames a = a.firstnames
  let lastname a = a.lastname
end

module Bibentry = struct
  type t =
  { entrytype: string
  ; key: string
  ; title: string
  ; author: Author.t list
  ; journal: string option
  ; volume: string option
  ; number: string option
  ; year: string option
  ; pages: string option
  ; url: string option
  ; arxiv: string option }

  let to_bib e =
    let otag tag f =
      match f with None -> "" | Some s -> "\t" ^ tag ^ " = {" ^ s ^ "},\n"
    in
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
    "@" ^ e.entrytype ^ "{" ^ e.key ^ ",\n"
    ^ ("\ttitle = {" ^ e.title ^ "},\n")
    ^ ("\tauthor = {" ^ (author_to_bib "" e.author)
      ^ "},\n")
    ^ (otag "journal" e.journal)
    ^ (otag "volume" e.volume)
    ^ (otag "number" e.number)
    ^ (otag "year" e.year)
    ^ (otag "pages" e.pages)
    ^ (otag "url" e.url)
    ^ (otag "arxiv" e.arxiv)
    ^ "}\n"
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
    match Bibtex.find_entry key db with
    | exception Caml.Not_found ->
        None
    | item -> (
      match item with
      | Comment _ | Preamble _ | Abbrev (_, _) ->
          None
      | Entry (entrytype, key, properties) ->
          let prop_get ?(fail = false) tag =
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
              | s ->
                  Some s )
          in
          Some (
            { entrytype
            ; key
            ; title=
                prop_get ~fail:true "title"
                |> (Option.value_exn : string option -> string)
            ; author=
                prop_get ~fail:true "author"
                |> (Option.value_exn : string option -> string) 
                |> Author.parse
            ; journal= prop_get "journal"
            ; volume= prop_get "volume"
            ; number= prop_get "number"
            ; year= prop_get "year"
            ; pages= prop_get "pages"
            ; url= prop_get "url"
            ; arxiv= prop_get "arxiv" } : Bibentry.t) )

end
