open Common

 let tag_with_tooltip tagname name classes tooltip_text display_text =
   let tooltip = !%{|<span class="tooltip-area markdown">%s</span>|} tooltip_text in
   !%{|<%s name="%s" class="%s tooltip">%s%s</%s>|} tagname name classes
     (html_escaped display_text) tooltip tagname
