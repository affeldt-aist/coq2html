open Common

let span_of_tooltip_text = function
  | Type_lookup.Markdown text ->
     !%{|<span class="tooltip-area markdown">%s</span>|} text
  | PlainText text ->
     !%{|<span class="tooltip-area">%s</span>|} text

 let tag_with_tooltip tagname name classes tooltip_text display_text =
   let tooltip = span_of_tooltip_text tooltip_text in
   !%{|<%s name="%s" class="%s tooltip">%s%s</%s>|} tagname name classes
     (html_escaped display_text) tooltip tagname
