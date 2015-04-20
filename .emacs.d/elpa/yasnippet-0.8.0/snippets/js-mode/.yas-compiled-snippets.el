;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("field" "///<field ${1:type}=\"$2\">$3</field>$0" "field" nil nil nil nil nil nil)
                       ("header" "/// <summary>${1}<\\summary>\n/// <param name=\"${2}\" type=\"${3}\">\n///    ${4}\n/// </param>\n/// <param name=\"${5}\" type=\"${6}\">\n///    ${7}\n/// </param>\n/// <param name=\"${8}\" type=\"${9}\">\n///    ${10}\n/// </param>\n/// <param name=\"${11}\" type=\"${12}\">\n///    ${13}\n/// </param>\n/// <param name=\"${14}\" type=\"${15}\">\n///    ${16}\n/// </param> $0" "header" nil nil nil nil nil nil)
                       ("param" "/// <param name=\"$1\" type=\"$2\">\n///    $0\n/// </param>\n" "param" nil nil nil nil nil nil)
                       ("summary" "/// <summary>$1<\\summary>$0" "summary" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Apr 17 15:15:59 2015
