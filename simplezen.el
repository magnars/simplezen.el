;;; simplezen.el --- A simple subset of zencoding-mode for Emacs.

;; Copyright (C) 2013 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple subset of zencoding-mode for Emacs.

;;; Code:

(require 'dash)
(require 's)

(defvar simplezen-html-tags
  '("a" "abbr" "acronym" "address" "area" "b" "base" "bdo" "big" "blockquote" "body" "br"
    "button" "caption" "cite" "code" "col" "colgroup" "dd" "del" "dfn" "div" "dl" "dt"
    "em" "fieldset" "form" "h1" "h2" "h3" "h4" "h5" "h6" "head" "html" "hr" "i" "img"
    "input" "ins" "kbd" "label" "legend" "li" "link" "map" "meta" "noscript" "object" "ol"
    "optgroup" "option" "p" "param" "pre" "q" "samp" "script" "select" "small" "span"
    "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea" "tfoot" "th" "thead"
    "title" "tr" "tt" "ul" "var" "article" "aside" "bdi" "command" "details" "dialog"
    "summary" "figure" "figcaption" "footer" "header" "hgroup" "mark" "meter" "nav"
    "progress" "ruby" "rt" "rp" "section" "time" "wbr" "audio" "video" "source" "embed"
    "track" "canvas" "datalist" "keygen" "output"))

(defvar simplezen-empty-tags
  '("area" "base" "basefont" "br" "col" "frame" "hr" "img" "input" "isindex" "link"
    "meta" "param" "wbr"))

(defvar simplezen-fallback-behavior nil
  "Function to call if simplezen does not find a match.")

(defun simplezen-expand ()
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (search-backward-regexp " \\|^")
                              (when (looking-at " ") (forward-char 1))
                              (point)))
         (expressions (s-slice-at "[.#]" (buffer-substring beg end)))
         (first (car expressions))
         (tagname (if (or (s-starts-with? "." first)
                          (s-starts-with? "#" first))
                      "div"
                    first)))
    (if (member tagname simplezen-html-tags)
        (let ((id (--first (s-starts-with? "#" it) expressions))
              (classes (->> expressions
                         (--filter (s-starts-with? "." it))
                         (--map (s-chop-prefix "." it))
                         (s-join " "))))
          (delete-char (- beg end))
          (insert "<" tagname
                  (if (s-blank? id) "" (s-concat " id=\"" (s-chop-prefix "#" id) "\""))
                  (if (s-blank? classes) "" (s-concat " class=\"" classes "\""))
                  ">")
          (unless (member tagname simplezen-empty-tags)
            (save-excursion (insert "</" tagname ">"))))
      (when simplezen-fallback-behavior
        (call-interactively simplezen-fallback-behavior)))))

(defun simplezen-expand-or-indent-for-tab ()
  (interactive)
  (let ((simplezen-fallback-behavior 'indent-for-tab-command))
    (simplezen-expand)))

(provide 'simplezen)
;;; simplezen.el ends here
