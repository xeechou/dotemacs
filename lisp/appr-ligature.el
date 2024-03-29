;;; appr-ligature.el --- ligature configuration for appr -*- lexical-binding: t
;;; -*-

;; Copyright (C) 2024 Xichen Zhou
;; Author: Xichen Zhou <sichem.zh@gmail.com>
;; Maintainer: Xichen Zhou <sichem.zh@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup appr-ligatures nil
  "appr ligature symbols."
  :group 'appr
  :group 'ligature)

(defvar appr-ligature-monolisa-symbols '("-->" "->" "->>" "-<" "--<"
					 "-~" "]#" ".-" "!=" "!=="
					 "#(" "#{" "#[" "#_" "#_("
					 "/=" "/==" "|||" "||" ;; "|"
					 "==" "===" "==>" "=>" "=>>"
					 "=<<" "=/" ">-" ">->" ">="
					 ">=>" "<-" "<--" "<->" "<-<"
					 "<!--" "<|" "<||" "<|||"
					 "<|>" "<=" "<==" "<==>" "<=>"
					 "<=<" "<<-" "<<=" "<~" "<~>"
					 "<~~" "~-" "~@" "~=" "~>"
					 "~~" "~~>" ".=" "..=" "---"
					 "{|" "[|" ".."  "..."  "..<"
					 ".?"  "::" ":::" "::=" ":="
					 ":>" ":<" ";;" "!!"  "!!."
					 "!!!"  "?."  "?:" "??"  "?="
					 "**" "***" "*>" "*/" "#:"
					 "#!"  "#?"  "##" "###" "####"
					 "#=" "/*" "/>" "//" "///"
					 "&&" "|}" "|]" "$>" "++"
					 "+++" "+>" "=:=" "=!=" ">:"
					 ">>" ">>>" "<:" "<*" "<*>"
					 "<$" "<$>" "<+" "<+>" "<>"
					 "<<" "<<<" "</" "</>" "^="
					 "%%" "'''" "\"\"\"" ))

(defvar appr-ligature-monolisa (cons "Monolisa" appr-ligature-monolisa-symbols))

(defvar appr-ligature-cascadia-symbols  '("|||>" "<|||" "<==>" "<!--" "####" "~~>"
					  "***" "||=" "||>" ":::" "::=" "=:=" "==="
					  "==>" "=!=" "=>>" "=<<" "=/=" "!==" "!!."
					  ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->"
					  "---" "-<<" "<~~" "<~>" "<*>" "<||" "<|>"
					  "<$>" "<==" "<=>" "<=<" "<->" "<--" "<-<"
					  "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_("
					  "..<" "..." "+++" "/==" "///" "_|_" "www"
					  "&&" "^=" "~~" "~@" "~=" "~>" "~-" "**"
					  "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-"
					  "{|" "[|" "]#" "::" ":=" ":>" ":<" "$>"
					  "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-"
					  "-~" "-|" "->" "--" "-<" "<~" "<*" "<|"
					  "<:" "<$" "<=" "<>" "<-" "<<" "<+" "</"
					  "#{" "#[" "#:" "#=" "#!" "##" "#(" "#?"
					  "#_" "%%" ".=" ".-" ".." ".?" "+>" "++"
					  "?:" "?=" "?." "??" ";;" "/*" "/=" "/>"
					  "//" "__" "~~" "(*" "*)" "\\\\" "://"))

(defvar appr-ligature-cascadia (cons "Cascadia Code" appr-ligature-cascadia-symbols))

(defvar appr-ligature-fira-symbols '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
				     "{-" "::" ":::" ":=" "!!" "!=" "!==" "-}" "----"
				     "-->" "->" "->>" "-<" "-<<" "-~" "#{" "#[" "##"
				      "###" "####" "#(" "#?" "#_" "#_(" ".-" ".=" ".."
				      "..<" "..." "?=" "??" ";;" "/*" "/**" "/=" "/=="
				      "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^="
				      "$>" "++" "+++" "+>" "=:=" "==" "===" "==>" "=>"
				      "=>>" "<=" "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-"
				      ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>"
				      "<!--" "<-" "<--" "<->" "<+" "<+>" "<=" "<=="
				      "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
				      "<~~" "</" "</>" "~@" "~-" "~>" "~~"
				      "~~>" "%%"))
(defvar appr-ligature-fira (cons "Fira Code" appr-ligature-fira-symbols))

(defvar appr-ligature-iosevka-symbols '("<---" "<--"  "<<-" "<-" "->" "-->" "--->"
					"<->" "<-->" "<--->" "<---->" "<!--"
					"<==" "<===" "<=" "=>" "=>>" "==>" "===>"
					">=" "<=>" "<==>" "<===>" "<====>" "<!---"
					"<~~" "<~" "~>" "~~>" "::" ":::" "==" "!="
					"===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>"
					"<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++"
					"+++"))

(defvar appr-ligature-iosevka (cons "Iosevka" appr-ligature-iosevka-symbols))

(defvar appr-ligature-jetbrain-symbols '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>"
					 "-->" "///" "/=" "/==" "/>" "//" "/*" "*>"
					 "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
					 "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<=="
					 "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|"
					 "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</"
					 "<*" "<*>" "<->" "<!--" ":>" ":<" ":::" "::"
					 ":?" ":?>" ":=" "::=" "=>>" "==>" "=/="
					 "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!="
					 ">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">="
					 "&&&" "&&" "|||>" "||>" "|>" "|]" "|}" "|=>"
					 "|->" "|=" "||-" "|-" "||=" "||" ".." ".?"
					 ".=" ".-" "..<" "..." "+++" "+>" "++" "[||]"
					 "[<" "[|" "{|" "??" "?." "?=" "?:" "##" "###"
					 "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_"
					 "#?" "#(" ";;" "_|_" "__" "~~" "~~>" "~>"
					 "~-" "~@" "$>" "^=" "]#"))

(defvar appr-ligature-jetbrain (cons "JetBrains Mono" appr-ligature-jetbrain-symbols))

(defvar appr-ligature-berkley-symbols
  '(
    ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
    ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"

    "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
    "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
    "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
    "|=" "//=" "/="

    "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
    "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
    "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"

    "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"

    "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
    "{!--" "//" "///" "!!"

    "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"

    "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
    "--" "---"))

(defvar appr-ligature-berkley (cons "Berkeley Mono" appr-ligature-berkley-symbols))

(provide 'appr-ligature)
