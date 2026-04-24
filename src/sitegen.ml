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
  hero_image : string;
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
      let right =
        String.sub s (idx + 1) (String.length s - idx - 1)
      in
      Some (left, right)

let split_kv line =
  match (split_once ':' line, split_once '=' line) with
  | Some (k, v), None -> Some (k, v)
  | None, Some (k, v) -> Some (k, v)
  | Some (k1, v1), Some (k2, v2) ->
      let colon_idx = String.length k1 in
      let equals_idx = String.length k2 in
      if colon_idx <= equals_idx then Some (k1, v1) else Some (k2, v2)
  | None, None -> None

let normalize_key key =
  key |> String.trim |> String.lowercase_ascii
  |> String.map (function '-' | ' ' -> '_' | c -> c)

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
    hero_image = get "hero_image" "assets/seminar-room.jpg";
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

let attr name value =
  if value = "" then "" else sprintf " %s=\"%s\"" name (escape_html value)

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

let render_date_box date =
  match parse_iso_date date with
  | Some (year, month, day) ->
      sprintf
        {|<div class="talk-date"><span class="month">%s</span><strong>%02d</strong><span class="year">%d</span></div>|}
        (month_name month) day year
  | None ->
      sprintf
        {|<div class="talk-date"><span class="month">Date</span><strong>%s</strong><span class="year">TBA</span></div>|}
        (safe_text (if date = "" then "TBA" else date))

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
  | speaker, "" -> safe_text speaker
  | "", affiliation -> safe_text affiliation
  | speaker, affiliation -> sprintf "%s, %s" (safe_text speaker) (safe_text affiliation)

let plain_speaker_line talk =
  match (talk.speaker, talk.affiliation) with
  | "", "" -> ""
  | speaker, "" -> speaker
  | "", affiliation -> affiliation
  | speaker, affiliation -> sprintf "%s, %s" speaker affiliation

let render_material_links talk =
  let links =
    (if talk.url = "" then [] else [ ("Details", talk.url) ]) @ talk.materials
  in
  match links with
  | [] -> ""
  | _ ->
      links
      |> List.map (fun (label, url) ->
             sprintf {|<a href="%s">%s</a>|} (escape_html url) (safe_text label))
      |> String.concat "\n"
      |> sprintf {|<div class="talk-links">%s</div>|}

let render_status status =
  if status = "" then "" else sprintf {|<span class="badge">%s</span>|} (safe_text status)

let render_talk_card talk =
  sprintf
    {|<li>
  <article class="talk-card">
    %s
    <div class="talk-body">
      <div class="talk-topline">%s<span>%s</span></div>
      <h3>%s</h3>
      <p class="speaker-line">%s</p>
      <p>%s</p>
      %s
    </div>
  </article>
</li>|}
    (render_date_box talk.date)
    (render_status talk.status)
    (safe_text (display_date talk.date))
    (safe_text talk.title) (speaker_line talk) (safe_text talk.abstract)
    (render_material_links talk)

let render_talk_list ?(limit = max_int) empty_message talks =
  let talks =
    talks |> List.mapi (fun idx talk -> (idx, talk))
    |> List.filter (fun (idx, _) -> idx < limit)
    |> List.map snd
  in
  match talks with
  | [] -> sprintf {|<div class="empty">%s</div>|} (safe_text empty_message)
  | _ ->
      talks |> List.map render_talk_card |> String.concat "\n"
      |> sprintf {|<ul class="talk-list">%s</ul>|}

let contact_link site =
  if site.email = "" then ""
  else sprintf {|<a href="mailto:%s">Email organizers</a>|} (escape_html site.email)

let optional_action label url =
  if url = "" then "" else sprintf {|<a class="button" href="%s">%s</a>|} (escape_html url) label

let optional_list_item label url =
  if url = "" then ""
  else sprintf {|<li><a href="%s">%s</a></li>|} (escape_html url) label

let nav_link active href label =
  let current = if active = href then {| aria-current="page"|} else "" in
  sprintf {|<a href="%s"%s>%s</a>|} href current label

let layout site ~active ~title body =
  let page_title =
    if title = site.name then site.name else sprintf "%s - %s" title site.name
  in
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
  <header class="site-header">
    <a class="brand" href="index.html" aria-label="%s home">
      <span class="brand-mark">%s</span>
      <span class="brand-name">%s</span>
    </a>
    <nav class="site-nav" aria-label="Main navigation">
      %s
      %s
      %s
      %s
    </nav>
  </header>
  %s
  <footer class="site-footer">
    <div class="footer-inner">
      <span>%s</span>
      <span>%s</span>
    </div>
  </footer>
</body>
</html>
|}
    (safe_text site.description) (safe_text page_title) (safe_text site.name)
    (safe_text site.short_name) (safe_text site.name)
    (nav_link active "index.html" "Home")
    (nav_link active "schedule.html" "Schedule")
    (nav_link active "archive.html" "Archive")
    (nav_link active "about.html" "About")
    body (safe_text site.department) (safe_text site.university)

let render_next_panel site talk_opt =
  match talk_opt with
  | None ->
      sprintf
        {|<section class="panel next-panel">
  <p class="kicker">Next seminar</p>
  <h2>Schedule coming soon</h2>
  <dl class="details">
    <div class="detail-row"><dt>Time</dt><dd>%s</dd></div>
    <div class="detail-row"><dt>Location</dt><dd>%s</dd></div>
  </dl>
  <p class="abstract">%s</p>
</section>|}
        (safe_text site.meeting_time) (safe_text site.location)
        (safe_text site.description)
  | Some talk ->
      let location = if talk.location = "" then site.location else talk.location in
      sprintf
        {|<section class="panel next-panel">
  <p class="kicker">Next seminar</p>
  <h2>%s</h2>
  <p class="speaker-line">%s</p>
  <dl class="details">
    <div class="detail-row"><dt>Date</dt><dd>%s</dd></div>
    <div class="detail-row"><dt>Time</dt><dd>%s</dd></div>
    <div class="detail-row"><dt>Location</dt><dd>%s</dd></div>
  </dl>
  <p class="abstract">%s</p>
  <div class="actions">
    %s
    %s
    %s
  </div>
</section>|}
        (safe_text talk.title) (speaker_line talk)
        (safe_text (display_date talk.date))
        (safe_text site.meeting_time) (safe_text location)
        (safe_text talk.abstract)
        (optional_action "Full schedule" "schedule.html")
        (optional_action "Add calendar" site.calendar_url)
        (optional_action "Propose a talk" site.submit_url)

let render_resource_sections site =
  let items =
    [
      optional_list_item "Join mailing list" site.mailing_list_url;
      optional_list_item "Add seminar calendar" site.calendar_url;
      optional_list_item "Propose a talk" site.submit_url;
      (if site.email = "" then ""
       else
         sprintf {|<li><a href="mailto:%s">Email organizers</a></li>|}
           (escape_html site.email));
      optional_list_item "GitHub repository" site.github_url;
    ]
    |> List.filter (( <> ) "")
  in
  let links =
    match items with
    | [] -> sprintf {|<p>%s</p>|} (safe_text site.description)
    | _ -> sprintf {|<ul class="link-list">%s</ul>|} (String.concat "\n" items)
  in
  sprintf
    {|<section class="link-panel">
    <p class="kicker">%s</p>
    <h2>%s</h2>
    <p>%s</p>
  </section>
  <section class="link-panel">
    <p class="kicker">Links</p>
    <h2>Organizer resources</h2>
    %s
  </section>|}
    (safe_text site.term) (safe_text site.meeting_time) (safe_text site.location)
    links

let render_side_panel site =
  sprintf {|<aside class="side-stack">%s</aside>|} (render_resource_sections site)

let home_page site talks =
  let upcoming = upcoming_talks talks in
  let body =
    sprintf
      {|<main>
  <section class="hero">
    <img src="%s" alt="">
    <div class="hero-content">
      <p class="eyebrow">%s</p>
      <h1>%s</h1>
      <div class="hero-meta">
        <span>%s</span>
        <span>%s</span>
      </div>
    </div>
  </section>
  <div class="wrap main-grid">
    <div>
      %s
      <section class="upcoming-section">
        <div class="section-head">
          <div>
            <h2>Upcoming talks</h2>
            <p>%s</p>
          </div>
          <a class="button" href="schedule.html">View all</a>
        </div>
        %s
      </section>
    </div>
    %s
  </div>
</main>|}
      (safe_text site.hero_image) (safe_text site.term) (safe_text site.name)
      (safe_text site.meeting_time) (safe_text site.location)
      (render_next_panel site (first_upcoming talks))
      (safe_text site.description)
      (render_talk_list ~limit:4 "No upcoming talks are listed yet." upcoming)
      (render_side_panel site)
  in
  layout site ~active:"index.html" ~title:site.name body

let schedule_page site talks =
  let upcoming = upcoming_talks talks in
  let body =
    sprintf
      {|<main class="wrap page-main">
  <div class="page-title">
    <h1>Schedule</h1>
    <p>%s - %s at %s.</p>
  </div>
  %s
</main>|}
      (safe_text site.term) (safe_text site.meeting_time) (safe_text site.location)
      (render_talk_list "No upcoming talks are listed yet." upcoming)
  in
  layout site ~active:"schedule.html" ~title:"Schedule" body

let archive_page site talks =
  let archived = archived_talks talks in
  let body =
    sprintf
      {|<main class="wrap page-main">
  <div class="page-title">
    <h1>Archive</h1>
    <p>Past talks and materials from %s.</p>
  </div>
  %s
</main>|}
      (safe_text site.name)
      (render_talk_list "No archived talks yet." archived)
  in
  layout site ~active:"archive.html" ~title:"Archive" body

let about_page site _talks =
  let contact =
    match contact_link site with
    | "" -> "Contact details TBA."
    | link -> link
  in
  let body =
    sprintf
      {|<main class="wrap page-main">
  <div class="page-title">
    <h1>About</h1>
    <p>%s</p>
  </div>
  <div class="about-grid">
    <div class="about-copy">
      <section class="panel">
        <p class="kicker">Format</p>
        <h2>Student-run, low overhead</h2>
        <p>Talks can be finished research, early ideas, practice conference talks, paper walkthroughs, or methods sessions.</p>
      </section>
      <section class="panel">
        <p class="kicker">Audience</p>
        <h2>Graduate students first</h2>
        <p>The seminar is built around graduate student participation, but visitors are welcome when a talk is open to the department.</p>
      </section>
    </div>
    <aside class="side-stack">
      <section class="link-panel">
        <p class="kicker">Organizers</p>
        <h2>%s</h2>
        <p>%s</p>
      </section>
      %s
    </aside>
  </div>
</main>|}
      (safe_text site.description) (safe_text site.short_name) contact
      (render_resource_sections site)
  in
  layout site ~active:"about.html" ~title:"About" body

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
               sprintf "Speaker: %s\n\n%s" (plain_speaker_line talk) talk.abstract
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
  write_file (output_dir // "schedule.html") (schedule_page site talks);
  write_file (output_dir // "archive.html") (archive_page site talks);
  write_file (output_dir // "about.html") (about_page site talks);
  write_file (output_dir // "404.html") (home_page site talks);
  generate_ics site talks
