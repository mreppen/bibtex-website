open Base
open Stdio

open Bibtools

let format_title ?(url = "") title =
  (match url with
  | "" ->
      title
  | link ->
      Printf.sprintf {|<a href="%s">%s</a>|} link title
  ) |> Printf.sprintf {|<span class="title"><q>%s</q></span>|}


let bibentry_to_string ~bibfile (entry : Bibentry.t) =
  let omap = Latex_expand.omap in
  let title =
    let url = omap entry.url in
    format_title ~url (Latex_expand.all entry.title)
  in
  let authors = Author.list_to_html entry.author in
  let publication_str = Publication.to_html entry.publication in
  let links =
    let arxiv_link =
      omap entry.arxiv ~f:(fun x ->
        let open Str in
        let link =
          if string_match (regexp {|^\(arXiv:\)?[0-9]+\.[0-9]+$|}) x 0 then "https://arxiv.org/abs/" ^ x
          else if string_match (regexp ({|^https?://arxiv.org/|})) x 0 then x
          else failwith ("arxiv entry not an arXiv identifier or arXiv link: " ^ x)
        in
        {|<a class="paperlink" href="|} ^ link ^ {|">arXiv</a>|})
    in
    let bib_link =
      Option.value_map bibfile ~default:"" ~f:(fun x ->
          {|<a class="bib paperlink" href="|} ^ x ^ {|">bib</a>|})
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
  Printf.sprintf {|<span class="publication">%s %s, by %s. %s</span>|} title publication_str authors links

let main bibdir args =
  let bibs =
    List.filter args
      ~f:Str.(fun s -> string_match (regexp {|.*\.bib$|}) s 0)
  and templates =
    List.filter args
      ~f:Str.(fun s -> string_match (regexp {|.*\.html$|}) s 0)
  in
  let db = Bibdb.of_files bibs in
  let rec parse_line s pos =
    let r = Str.regexp {|<bibtex item="\([A-Za-z0-9-_]+\)" */?>|} in
    let i = try Str.search_forward r s pos with Stdlib.Not_found -> -1 in
    if i >= pos then (
      Out_channel.output_substring Out_channel.stdout ~buf:s ~pos ~len:(i - pos) ;
      let tag_end = Str.match_end () in
      let key = Str.matched_group 1 s in
      match Bibdb.find_entry db key with
      | None ->
          failwith ("Entry " ^ key ^ " not found")
      | Some e ->
          let bibfile = Option.map bibdir ~f:(fun x -> 
                let bibfile = x ^ "/" ^ e.key ^ ".bib" in
                Out_channel.write_all bibfile ~data:(Bibentry.to_bib e);
                bibfile)
          in
          bibentry_to_string ~bibfile e
          |> Out_channel.output_string Out_channel.stdout ;
          parse_line s tag_end )
    else (
      Out_channel.output_substring Out_channel.stdout ~buf:s ~pos
        ~len:(String.length s - pos) ;
      Out_channel.newline Out_channel.stdout )
  in
  begin
    match bibdir with None -> ()
    | Some bibdir ->
      try Unix.mkdir bibdir (Unix.stat ".").st_perm
      with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end ;
  List.iter
    ~f:(fun template ->
      In_channel.create template
      |> In_channel.iter_lines ~f:(fun x -> parse_line x 0))
    templates



let () =
  let open Cmdliner in
  let bibdir =
    let doc = "Creates separate .bib files in $(docv) for each entry, and links them in the HTML output." in
    Arg.(value & opt (some string) None & info ["bibdir"] ~docv:"BIBDIR" ~doc)
  in
  let files =
    let doc = "List of .bib files to read bibliography data and .html files to replace in." in
    Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)
  in
  let cmd =
    let doc = "Replaced bibtex tags in HTML with references" in
    let man = [] in
    Term.(const main $ bibdir $ files),
    Term.info "bibtex-website" ~version:"dev" ~doc ~exits:Term.default_exits ~man
  in
  Term.(exit @@ eval cmd)

