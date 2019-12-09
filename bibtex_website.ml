open Base
open Stdio

open Bibtools

let format_title ?(url = "") title =
  match url with
  | "" ->
      title
  | link ->
      {|<a href="|} ^ link ^ {|">|} ^ title ^ "</a>"


let bibentry_to_string ?bibfile (entry : Bibentry.t) =
  let omap ?(f = fun x -> x) o =
    Option.value_map ~default:"" ~f:(fun x -> Latex_expand.all x |> f) o
  in
  let title =
    let url = omap entry.url in
    format_title ~url (Latex_expand.all entry.title)
  in
  let authors = Author.list_to_string entry.author |> Latex_expand.all in
  let journal = omap entry.journal in
  let volume = omap entry.volume ~f:(( ^ ) " ") in
  let number = omap entry.number ~f:(( ^ ) ", no.&nbsp;") in
  let year = omap entry.year ~f:(fun s -> " (" ^ s ^ ")") in
  let pages =
    omap entry.pages ~f:(fun x ->
        (* TODO: parse pages *)
        ": " ^ x |> Str.(global_replace (regexp "-+") "–"))
  in
  let publication_data =
    match journal ^ volume ^ number ^ year ^ pages with
    | "" ->
        ""
    | s ->
        s ^ "."
  in
  let links =
    let arxiv_link =
      omap entry.arxiv ~f:(fun x ->
        let open Str in
        let link =
          if string_match (regexp {|^\(arXiv:\)?[0-9]+\.[0-9]+$|}) x 0 then "https://arxiv.org/abs/" ^ x
          else if string_match (regexp ({|^https?://arxiv.org/|})) x 0 then x
          else failwith ("arxiv entry not an arXiv identifier or arXiv link: " ^ x)
        in
        {|<a href="|} ^ link ^ {|">arXiv</a>|})
    in
    let bib_link =
      Option.value_map bibfile ~default:"" ~f:(fun x ->
          {|<a href="|} ^ x ^ {|">bib</a>|})
    in
    List.filter ~f:(fun s -> not (String.equal s "")) [bib_link; arxiv_link]
    |> function
    | [] ->
        ""
    | [hd] ->
        " [&nbsp;" ^ hd ^ "&nbsp;]"
    | hd :: tl ->
        " [&nbsp;"
        ^ List.fold ~init:hd ~f:(fun acc s -> acc ^ " | " ^ s) tl
        ^ "&nbsp;]"
  in
  authors ^ {|. "|} ^ title ^ {|." |} ^ publication_data ^ links

let () =
  let args = Sys.argv |> Array.to_list in
  let bibs =
    List.filter (List.tl_exn args)
      ~f:Str.(fun s -> string_match (regexp {|.*\.bib$|}) s 0)
  and templates =
    List.filter (List.tl_exn args)
      ~f:Str.(fun s -> string_match (regexp {|.*\.html$|}) s 0)
  in
  let db = Bibdb.of_files bibs in
  let rec parse_line s pos =
    let r = Str.regexp {|<bibtex item="\([A-Za-z0-9-_]+\)" */?>|} in
    let i = try Str.search_forward r s pos with Caml.Not_found -> -1 in
    if i >= pos then (
      Out_channel.output_substring Out_channel.stdout ~buf:s ~pos ~len:(i - pos) ;
      let tag_end = Str.match_end () in
      let key = Str.matched_group 1 s in
      match Bibdb.find_entry db key with
      | None ->
          failwith ("Entry " ^ key ^ " not found")
      | Some e ->
          let bibfile = "bib/" ^ e.key ^ ".bib" in
          bibentry_to_string ~bibfile e
          |> Out_channel.output_string Out_channel.stdout ;
          Out_channel.write_all bibfile ~data:(Bibentry.to_bib e) ;
          parse_line s tag_end )
    else (
      Out_channel.output_substring Out_channel.stdout ~buf:s ~pos
        ~len:(String.length s - pos) ;
      Out_channel.newline Out_channel.stdout )
  in
  ( try Unix.mkdir "bib" (Unix.stat ".").st_perm
    with Unix.Unix_error (Unix.EEXIST, _, _) -> () ) ;
  List.iter
    ~f:(fun template ->
      In_channel.create template
      |> In_channel.iter_lines ~f:(fun x -> parse_line x 0))
    templates
