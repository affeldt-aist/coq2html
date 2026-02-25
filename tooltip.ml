open Common

let span_of_tooltip content =
  !%"<span class='tooltip-area'>%s</span>" content

let tag_with_tooltip tagname name classes tooltip_content display_text =
  let tooltip = span_of_tooltip tooltip_content in
  !%{|<%s name="%s" class="%s tooltip">%s%s</%s>|} tagname name classes
    (html_escaped display_text) tooltip tagname
