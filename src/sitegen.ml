open Printf

type site = {
  name : string;
  short_name : string;
  term : string;
  meeting_time : string;
  location : string;
  department : string;
  university : string;
  timezone : string;
  description : string;
  email : string;
  mailing_list_url : string;
  calendar_url : string;
  submit_url : string;
  github_url : string;
}

type talk = {
  date : string;
  speaker : string;
  affiliation : string;
  title : string;
  abstract : string;
  location : string;
  url : string;
  status : string;
  materials : (string * string) list;
}

let output_dir = "dist"
let ( // ) = Filename.concat

let read_lines path =
  let channel = open_in path in
  let rec loop acc =
    match input_line channel with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in channel;
        List.rev acc
  in
  loop []

let write_file path contents =
  let channel = open_out path in
  output_string channel contents;
  close_out channel

let ensure_dir path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755

let is_blank s = String.trim s = ""

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let split_once ch s =
  match String.index_opt s ch with
  | None -> None
  | Some idx ->
      let left = String.sub s 0 idx in
      let right = String.sub s (idx + 1) (String.length s - idx - 1) in
      Some (left, right)

let normalize_key key =
  key |> String.trim |> String.lowercase_ascii
  |> String.map (function '-' | ' ' -> '_' | c -> c)

let split_kv line =
  match (split_once ':' line, split_once '=' line) with
  | Some (k, v), None -> Some (k, v)
  | None, Some (k, v) -> Some (k, v)
  | Some (k1, v1), Some (k2, v2) ->
      if String.length k1 <= String.length k2 then Some (k1, v1)
      else Some (k2, v2)
  | None, None -> None

let parse_site path =
  let pairs =
    read_lines path
    |> List.filter_map (fun line ->
           let line = String.trim line in
           if is_blank line || starts_with ~prefix:"#" line then None
           else
             match split_kv line with
             | None -> None
             | Some (key, value) ->
                 Some (normalize_key key, String.trim value))
  in
  let get key fallback =
    match List.assoc_opt key pairs with Some value -> value | None -> fallback
  in
  {
    name = get "name" "Graduate Student Seminar";
    short_name = get "short_name" "GSS";
    term = get "term" "Current Term";
    meeting_time = get "meeting_time" "Time TBA";
    location = get "location" "Location TBA";
    department = get "department" "Department Name";
    university = get "university" "University Name";
    timezone = get "timezone" "America/New_York";
    description =
      get "description"
        "A student-run seminar for graduate students to share research and ideas.";
    email = get "email" "";
    mailing_list_url = get "mailing_list_url" "";
    calendar_url = get "calendar_url" "";
    submit_url = get "submit_url" "";
    github_url = get "github_url" "";
  }

let split_tabs line = String.split_on_char '\t' line

let field fields idx =
  match List.nth_opt fields idx with Some value -> String.trim value | None -> ""

let parse_materials value =
  value |> String.split_on_char ';'
  |> List.filter_map (fun item ->
         let item = String.trim item in
         if item = "" then None
         else
           match split_once '|' item with
           | Some (label, url) -> Some (String.trim label, String.trim url)
           | None -> Some (item, item))

let parse_talks path =
  let rows =
    read_lines path
    |> List.filter (fun line ->
           let line = String.trim line in
           not (is_blank line || starts_with ~prefix:"#" line))
  in
  match rows with
  | [] -> []
  | header :: data_rows ->
      let header = split_tabs header |> List.map normalize_key in
      let index_of name =
        let rec loop idx = function
          | [] -> None
          | value :: rest -> if value = name then Some idx else loop (idx + 1) rest
        in
        loop 0 header
      in
      let get fields name fallback =
        match index_of name with
        | Some idx -> (
            match field fields idx with "" -> fallback | value -> value)
        | None -> fallback
      in
      data_rows
      |> List.map (fun row ->
             let fields = split_tabs row in
             {
               date = get fields "date" "TBA";
               speaker = get fields "speaker" "TBA";
               affiliation = get fields "affiliation" "";
               title = get fields "title" "Title TBA";
               abstract = get fields "abstract" "Abstract TBA.";
               location = get fields "location" "";
               url = get fields "url" "";
               status = get fields "status" "";
               materials = get fields "materials" "" |> parse_materials;
             })

let escape_html s =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#39;"
      | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let safe_text = escape_html

let is_iso_date date =
  String.length date = 10
  && date.[4] = '-'
  && date.[7] = '-'
  && String.for_all
       (fun c -> (c >= '0' && c <= '9') || c = '-')
       date

let month_name = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> ""

let parse_iso_date date =
  if is_iso_date date then
    try
      let year = int_of_string (String.sub date 0 4) in
      let month = int_of_string (String.sub date 5 2) in
      let day = int_of_string (String.sub date 8 2) in
      Some (year, month, day)
    with Failure _ -> None
  else None

let display_date date =
  match parse_iso_date date with
  | Some (year, month, day) -> sprintf "%s %d, %d" (month_name month) day year
  | None -> if date = "" then "TBA" else date

let today_iso () =
  let tm = Unix.localtime (Unix.time ()) in
  sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let dated_compare a b =
  match (parse_iso_date a.date, parse_iso_date b.date) with
  | Some _, Some _ -> String.compare a.date b.date
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let upcoming_talks talks =
  let today = today_iso () in
  talks
  |> List.filter (fun talk ->
         match parse_iso_date talk.date with
         | Some _ -> String.compare talk.date today >= 0
         | None -> true)
  |> List.sort dated_compare

let archived_talks talks =
  let today = today_iso () in
  talks
  |> List.filter (fun talk ->
         match parse_iso_date talk.date with
         | Some _ -> String.compare talk.date today < 0
         | None -> false)
  |> List.sort (fun a b -> -dated_compare a b)

let first_upcoming talks =
  match upcoming_talks talks with [] -> None | talk :: _ -> Some talk

let speaker_line talk =
  match (talk.speaker, talk.affiliation) with
  | "", "" -> ""
  | speaker, "" -> speaker
  | "", affiliation -> affiliation
  | speaker, affiliation -> sprintf "%s, %s" speaker affiliation

let html_speaker_line talk = speaker_line talk |> safe_text

let optional_link label url =
  if url = "" then None
  else Some (sprintf {|<a href="%s">%s</a>|} (escape_html url) (safe_text label))

let required_mail_link site =
  if site.email = "" then None
  else Some (sprintf {|<a href="mailto:%s">email</a>|} (escape_html site.email))

let join_with_bars links = String.concat " | " links

let render_materials talk =
  let links =
    (if talk.url = "" then [] else [ ("details", talk.url) ]) @ talk.materials
  in
  match links with
  | [] -> ""
  | _ ->
      links
      |> List.map (fun (label, url) ->
             sprintf {|<a href="%s">%s</a>|} (escape_html url) (safe_text label))
      |> join_with_bars
      |> sprintf "<br>%s"

let render_status status =
  if status = "" then "" else sprintf " (%s)" (safe_text status)

let render_talk_detail talk =
  let location =
    if talk.location = "" then "" else sprintf "<br>%s" (safe_text talk.location)
  in
  sprintf
    {|<dt><time%s>%s</time>%s</dt>
<dd><b>%s</b><br>%s<br>%s%s%s</dd>|}
    (match parse_iso_date talk.date with
    | Some _ -> sprintf {| datetime="%s"|} (safe_text talk.date)
    | None -> "")
    (safe_text (display_date talk.date))
    (render_status talk.status)
    (safe_text talk.title) (html_speaker_line talk) (safe_text talk.abstract)
    location (render_materials talk)

let render_talk_dl empty_message talks =
  match talks with
  | [] -> sprintf {|<p class="quiet">%s</p>|} (safe_text empty_message)
  | _ -> talks |> List.map render_talk_detail |> String.concat "\n" |> sprintf "<dl>%s</dl>"

let render_schedule_table empty_message talks =
  match talks with
  | [] -> sprintf {|<p class="quiet">%s</p>|} (safe_text empty_message)
  | _ ->
      let rows =
        talks
        |> List.map (fun talk ->
               let speaker = speaker_line talk in
               let location =
                 if talk.location = "" then "" else sprintf "<br>%s" (safe_text talk.location)
               in
               sprintf
                 {|<tr><td>%s</td><td>%s</td><td><b>%s</b><br>%s%s</td><td>%s</td></tr>|}
                 (safe_text (display_date talk.date))
                 (safe_text speaker)
                 (safe_text talk.title) (safe_text talk.abstract) location
                 (safe_text talk.status))
        |> String.concat "\n"
      in
      sprintf
        {|<table>
<thead><tr><th>Date</th><th>Speaker</th><th>Talk</th><th>Status</th></tr></thead>
<tbody>
%s
</tbody>
</table>|}
        rows

let render_links site =
  [
    optional_link "calendar" site.calendar_url;
    optional_link "mailing list" site.mailing_list_url;
    required_mail_link site;
    optional_link "propose a talk" site.submit_url;
    optional_link "github" site.github_url;
  ]
  |> List.filter_map Fun.id
  |> function
  | [] -> {|<p class="quiet">Links TBA.</p>|}
  | links -> sprintf {|<p>%s</p>|} (join_with_bars links)

let layout site body =
  sprintf
    {|<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="description" content="%s">
  <title>%s</title>
  <link rel="stylesheet" href="assets/styles.css">
</head>
<body>
%s
</body>
</html>
|}
    (safe_text site.description) (safe_text site.name) body

let home_page site talks =
  let upcoming = upcoming_talks talks in
  let archived = archived_talks talks in
  let next =
    match first_upcoming talks with
    | None -> {|<p class="quiet">Schedule coming soon.</p>|}
    | Some talk -> render_talk_dl "Schedule coming soon." [ talk ]
  in
  let body =
    sprintf
      {|<header>
  <h1>%s</h1>
  <p>%s</p>
  <p>%s<br>%s</p>
  <nav class="links">
    <a href="#next">next</a> | <a href="#schedule">schedule</a> | <a href="#archive">archive</a> | <a href="#about">about</a> | <a href="#links">links</a>
  </nav>
</header>

<hr>
<h2 id="next">next</h2>
%s

<hr>
<h2 id="schedule">schedule</h2>
%s

<hr>
<h2 id="archive">archive</h2>
%s

<hr>
<h2 id="about">about</h2>
<p>%s</p>
<p><b>format:</b> finished research, early ideas, practice talks, paper walkthroughs, or methods sessions.</p>
<p><b>audience:</b> graduate students first; visitors welcome when talks are open to the department.</p>
<p><b>organizers:</b> %s</p>

<hr>
<h2 id="links">links</h2>
%s

<hr>
<footer class="quiet">%s | %s</footer>
|}
      (safe_text site.name) (safe_text site.description)
      (safe_text site.term)
      (safe_text (sprintf "%s, %s" site.meeting_time site.location))
      next
      (render_schedule_table "No upcoming talks are listed yet." upcoming)
      (render_talk_dl "No archived talks yet." archived)
      (safe_text site.description)
      (match required_mail_link site with Some link -> link | None -> "TBA")
      (render_links site) (safe_text site.department) (safe_text site.university)
  in
  layout site body

let slugify s =
  let buffer = Buffer.create (String.length s) in
  let last_dash = ref false in
  String.lowercase_ascii s
  |> String.iter (fun c ->
         let is_alnum = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
         if is_alnum then (
           Buffer.add_char buffer c;
           last_dash := false)
         else if not !last_dash then (
           Buffer.add_char buffer '-';
           last_dash := true));
  Buffer.contents buffer |> String.trim

let ics_escape s =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buffer "\\\\"
      | ';' -> Buffer.add_string buffer "\\;"
      | ',' -> Buffer.add_string buffer "\\,"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> ()
      | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let compact_date date =
  match parse_iso_date date with
  | Some (year, month, day) -> sprintf "%04d%02d%02d" year month day
  | None -> ""

let timestamp_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  sprintf "%04d%02d%02dT%02d%02d%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let generate_ics site talks =
  if site.calendar_url = "" then ()
  else
    let dated_talks =
      talks
      |> List.filter (fun talk -> parse_iso_date talk.date <> None)
      |> List.sort dated_compare
    in
    let dtstamp = timestamp_utc () in
    let events =
      dated_talks
      |> List.map (fun talk ->
             let date = compact_date talk.date in
             let location = if talk.location = "" then site.location else talk.location in
             let description =
               sprintf "Speaker: %s\n\n%s" (speaker_line talk) talk.abstract
             in
             sprintf
               {|BEGIN:VEVENT
UID:%s-%s@graduate-student-seminar
DTSTAMP:%s
DTSTART;VALUE=DATE:%s
SUMMARY:%s
LOCATION:%s
DESCRIPTION:%s%s
END:VEVENT|}
               date
               (slugify talk.title) dtstamp date (ics_escape talk.title)
               (ics_escape location) (ics_escape description)
               (if talk.url = "" then "" else sprintf "\nURL:%s" (ics_escape talk.url)))
      |> String.concat "\n"
    in
    let calendar =
      sprintf
        {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//%s//Seminar Website//EN
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:%s
X-WR-TIMEZONE:%s
%s
END:VCALENDAR
|}
        (ics_escape site.name) (ics_escape site.name) (ics_escape site.timezone) events
    in
    write_file (output_dir // site.calendar_url) calendar

let () =
  let site = parse_site "data/site.txt" in
  let talks = parse_talks "data/talks.tsv" in
  ensure_dir output_dir;
  write_file (output_dir // ".nojekyll") "";
  write_file (output_dir // "index.html") (home_page site talks);
  write_file (output_dir // "404.html") (home_page site talks);
  generate_ics site talks
