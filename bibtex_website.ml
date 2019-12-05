open Base
open Stdio

module Bibtex = Bibtex2html_lib.Bibtex
module Readbib = Bibtex2html_lib.Readbib

let string_of_atoms ?prepend:(p="") ?append:(ap="") atoms =
  (List.fold ~f:(fun s a -> s ^ match a with
    | Bibtex.Id a -> a
    | Bibtex.String a -> a) ~init:p
    atoms |> String.split_on_chars ~on:['{'; '}'] |> String.concat)
  ^ ap

type author = { firstnames : string list; lastname : string }
let parse_authors field =
  let parse_author s =
    match String.lsplit2 ~on:',' s with
    | None -> { firstnames=[]; lastname=s }
    | Some (last, first) -> begin
      let first = String.split_on_chars ~on:[' '; '.'] first
      |> List.filter ~f:(fun s -> String.length s > 0) in
      { firstnames=first; lastname=last }
    end
  in
  Str.split (Str.regexp "[ \t\n]+and[ \t\n]+") field
  |> List.map ~f:parse_author

let format_authors l =
  let format_author a =
    let initials = List.map ~f:(fun s -> String.(get s 0 |> of_char) ^ ". ") a.firstnames
      |> String.concat in
    initials ^ a.lastname
  in
  let rec format_authors_rec l acc =
    match l with
    | [] -> acc
    | h::[] -> acc ^ ", and " ^ (format_author h)
    | h::t -> format_authors_rec t (acc ^ ", " ^ (format_author h))
  in
  match l with
  | [] -> ""
  | h::t -> format_authors_rec t (format_author h)


let format_title ?url:(url="") title =
  let title = String.substr_replace_all ~pattern:"--" ~with_:"–" title in
  match url with
  | "" -> title
  | link -> {|<a href="|} ^ link ^ {|">|} ^ title ^ "</a>"

let item_to_string db key =
  let item = Bibtex.find_entry key db in
  match item with
  | (Comment _|Preamble _|Abbrev (_, _)) -> ""
  | Entry (_etype, _key, properties) ->
      let prop_map ?f:(f=(fun x -> x)) ?fail:(fail=false) tag =
        let missing_field_errmsg = tag ^ " field not found or empty in item " ^ key in
        match List.Assoc.find ~equal:String.equal properties tag with
        | None -> if fail then failwith missing_field_errmsg else ""
        | Some s -> begin match string_of_atoms s with
          | "" -> if fail then failwith missing_field_errmsg else ""
          | s  -> f s
        end
      in
      let authors = prop_map "author" ~fail:true ~f:(fun x -> parse_authors x |> format_authors) in
      let title = 
        let title = prop_map "title" ~fail:true in
        let url = prop_map "url" in
        format_title ~url title in
      let journal = prop_map "journal" ~f:((^) " ") in
      let volume = prop_map "volume" ~f:((^) " ") in
      let number = prop_map "number" ~f:((^) ", no. ") in
      let year = prop_map "year" ~f:(fun s -> " (" ^ s ^ ")") in
      let pages = prop_map "pages" ~f:(fun x -> (* TODO: parse pages *)
        ": " ^ x |> Str.(global_replace (regexp "-+") "–")) in
      let arxiv_link = prop_map "arxiv" ~f:(fun x ->
        {| [<a href="|} ^ x ^ {|">arXiv</a>]|}) in
      authors ^ {|. "|} ^ title ^ {|." |}
      ^ journal ^ volume ^ number ^ year ^ pages ^ "." ^ arxiv_link

let () =
  let args = Sys.argv |> Array.to_list in
  let bibs = List.filter ~f:Str.(fun s -> string_match (regexp {|.*\.bib|}) s 0) (List.tl_exn args) in
  let templates = List.filter ~f:Str.(fun s -> string_match (regexp {|.*\.html|}) s 0) (List.tl_exn args) in
  let db = List.fold ~init:Bibtex.empty_biblio ~f:(fun b fp -> 
    Bibtex.merge_biblios b (Readbib.read_entries_from_file fp)) bibs in
  let rec parse_line s pos =
    let r = Str.regexp {|<bibtex item="\([A-Za-z0-9-_]+\)" */?>|} in
    let i = (try Str.search_forward r s pos
             with Caml.Not_found -> -1) in
    if i >= pos then (
      Out_channel.output_substring Out_channel.stdout ~buf:s ~pos ~len:(i - pos);
      let tag_end = Str.match_end () in
      let key = Str.matched_group 1 s in
      item_to_string db key
      |> Out_channel.output_string Out_channel.stdout;
      parse_line s tag_end
    )
    else (
      Out_channel.output_substring Out_channel.stdout ~buf:s ~pos ~len:((String.length s) - pos);
      Out_channel.newline Out_channel.stdout
    )
  in
  List.iter ~f:(fun template ->
    In_channel.create template
    |> In_channel.iter_lines ~f:(fun x -> parse_line x 0)) templates
