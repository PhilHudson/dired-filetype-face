;;; dired-filetype-face.el  --- set different faces for different filetypes in dired.

;; Copyright (C) 2011~2015, 纪秀峰(Joseph) all rights reserved.
;; Created: 2011-04-04
;; Author: 纪秀峰(Joseph) <jixiuf@gmail.com>
;; Contributor:Phil Hudson
;; Version: 0.3.0
;; URL: http://www.emacswiki.org/emacs/download/dired-filetype-face.el
;; X-URL:https://github.com/jixiuf/dired-filetype-face
;; Keywords: dired filetype face custom
;; Compatibility: (Test on GNU Emacs 23.2.1 ,24.0.50)
;;
;; Features that might be required by this library:
;;
;;   None
;;
;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  Set faces for different file types in dired. I use a dark background,
;;  so maybe the default face doesn't meet your request.
;;  You can:
;;
;;    M-x customize-group dired-filetype-face  RET
;;
;;  And maybe:
;;
;;    M-x customize-group dired-faces  RET
;;
;;  may do some help for you.
;;
;;
;;; Installation:
;;
;; Put `dired-filetype-face.el' in your load-path.
;; Your load-path might include the directory ~/elisp/, for example.
;; It's set in your ~/.emacs like this:
;;
;;   (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; Add the following to your ~/.emacs startup file.
;;
;;   (with-eval-after-load 'dired  (require 'dired-filetype-face))
;;
;; If you want to add a new face for new filetype(s):
;;
;;   (deffiletype-face "mytype" "Chartreuse")
;;
;; then either:
;;
;;   (deffiletype-face-regexp mytype
;;     :extensions '("foo" "bar") :type-for-docstring "my type")
;;
;; to match all files ending either ".foo" or ".bar", or equivalently:
;;
;;   (deffiletype-face-regexp mytype
;;     :regexp "^  -.*\\.\\(foo\\|bar\\)$" :type-for-docstring "my type")
;;
;; and finally:
;;
;;   (deffiletype-setup "mytype" "mytype")
;;
;; The :regexp form allows you to specify other things to match on each line of
;; the dired buffer than (only) file extensions, such as the permission bits,
;; the size and the modification times.
;;
;; No need more.
;;

;;; Code:

(eval-when-compile
    (require 'anaphora)
)

(require 'dired)
(require 'custom)

(autoload #'cl-reduce "cl-seq")

(defgroup dired-filetype-face nil
  "Set faces for different filetypes in dired."
  :prefix "dired-filetype-face-"
  :group 'dired-faces)

(defmacro i__d__f (fmt sym)
  "Call `format' on FMT and SYM, then `downcase', then `intern'."
  `(intern (downcase (format ,fmt ,sym))))

(defmacro deffiletype-face (type color &optional type-for-symbol)
  "Declare a dired filetype face for displaying TYPE files in the given COLOR.

If TYPE-FOR-SYMBOL is nil, define a face named
  dired-filetype-TYPE

Otherwise, define a face named
  dired-filetype-TYPE-FOR-SYMBOL

COLOR may be a string or a list of face properties. If a string,
it is either a color name such as \"Chartreuse\" or a color
hexadecimal RGB number such as \"#xaaaaaa\"."
  `(defface ,(i__d__f "dired-filetype-%s" (or type-for-symbol type))
     ,(if (stringp color)
       `(quote ((t (:foreground ,color))))
       color)
     ,(format "Face for displaying %s files in dired." type)
     :tag ,(format "Dired %s filetype face" type)
     :group 'dired-filetype-face))

(defmacro deffiletype-face-components (type-for-symbol &rest args)
    "Define a regexp option to colorize matching files in dired.

TYPE-FOR-SYMBOL is a symbol to splice into the defined option
symbol. The format string used in splicing is
\"dired-filetype-%s-regexp\", where %s will be replaced by
TYPE-FOR-SYMBOL.

The remaining arguments are keyword arguments accessed as ARGS.

Exactly one of the two mutually-exclusive keyword
arguments :regexp or :extensions is required.

Keyword argument :extensions must be a list of strings, each of
which is a literal filetype extension without a leading dot and
with no globbing or regexp syntax. This list will be used to
derive a regexp to match against each complete line in the dired
buffer.

Keyword argument :regexp must be a regexp string to match against
each complete line in the dired buffer. Use this to match file
names by something other than (only) the literal extension,
and/or by other attributes available in the dired buffer such as
modification timestamp and/or permission flags.

Optional keyword argument :type-for-docstring is either a symbol
or a string to splice into the user option docstring instead of
TYPE-FOR-SYMBOL. The format string used in splicing is \"Regexp
to match %s file-types in dired.\", where %s will be replaced by
keyword argument :type-for-docstring if given, or else by
TYPE-FOR-SYMBOL."
    (let*
        (
            (type-for-docstring
                (or (plist-get args :type-for-docstring)
                    type-for-symbol))
            (regexps
                (awhen (plist-get args :regexps)
                    (eval `(cons 'Regexps ,it))))
            (filenames
                (awhen (plist-get args :filenames)
                    (eval `(cons 'Filenames ,it))))
            (extensions
                (awhen (plist-get args :extensions)
                    (eval `(cons 'Extensions ,it))))
            value)

        (when regexps (add-to-list 'value regexps))
        (when filenames (add-to-list 'value filenames))
        (when extensions (add-to-list 'value extensions))
        (unless value
            (error
                "At least one of the keyword arguments :regexps, :filenames and :extensions is required"))

        `(defcustom ,(i__d__f "dired-filetype-%s-regexp" type-for-symbol)
             '(,@value)
             ,(format
                  "Either a list of file extensions or a regexp to match %s file-types in dired."
                  type-for-docstring)
             :type
             '(set
                  (cons :tag "Regexps" (const Regexps) (repeat regexp))
                  (cons :tag "Filenames" (const Filenames) (repeat string))
                  (cons :tag "Extensions" (const Extensions) (repeat string)))
             :tag ,(format "Dired %s filetype pattern" type-for-docstring)
             :group 'dired-filetype-face)))

(defmacro deffiletype-face-regexp (type-for-symbol regexp &optional type-for-docstring)
    "Define a regexp option to colorize matching files in dired.

TYPE-FOR-SYMBOL is a symbol to splice into the defined option
symbol. The format string used in splicing is
\"dired-filetype-%s-regexp\", where %s will be replaced by
TYPE-FOR-SYMBOL.

REGEXP must be a regexp string to match against each complete
line in the dired buffer. Use this to match file names by
something other than (only) the literal extension, and/or by
other attributes available in the dired buffer such as
modification timestamp and/or permission flags.

Optional argument TYPE-FOR-DOCSTRING is either a symbol
or a string to splice into the user option docstring instead of
TYPE-FOR-SYMBOL. The format string used in splicing is \"Regexp
to match %s file-types in dired.\", where %s will be replaced by
keyword argument :type-for-docstring if given, or else by
TYPE-FOR-SYMBOL."
    (let
        ((type-for-docstring (or type-for-docstring type-for-symbol)))
        `(defcustom ,(i__d__f "dired-filetype-%s-regexp" type-for-symbol)
             ,regexp
             ,(format
                  "Regexp to match %s file-types in dired." type-for-docstring)
             :type
             '(regexp
                  :tag "Regular expression to match against whole dired line"
                  :format "%t\n%v%h"
                  :doc "Include two leading spaces, like this: \"^  \".")
             :tag ,(format "Dired %s filetype pattern" type-for-docstring)
             :group 'dired-filetype-face)))

(defconst dired-filetype-face-font-lock-keywords
  '(("(\\(deffiletype\\(?:-\\(?:face\\(?:-\\(?:regexp\\|components\\)\\)?\\|setup\\)\\)?\\)\\_>"
     (1 font-lock-keyword-face))))

(font-lock-add-keywords 'emacs-lisp-mode dired-filetype-face-font-lock-keywords)

(defvar dired-filetype-setup-hook nil)

(deffiletype-face "omit" "dark gray")

(deffiletype-face-components omit1
  :type-for-docstring unimportant
  :extensions
  '(
     "al"
     "bak"
     "cat"
     "class"
     "dat"
     "db"
     "DLL"
     "Dll"
     "dll"
     "elc"
     "fas"
     "fasl"
     "ix"
     "ko"
     "la"
     "o"
     "prf"
     "rdp"
     "sav"
     "so"
     "SYS"
     "sys"
     "td"
     "tlb"))

;; TODO Enable directories
(deffiletype-face-components omit2
    :type-for-docstring "backup or cache"
    :filenames
    '(
         "#"
         "$DATA"
         "%"
         ":encryptable"
         "~"
    )
    :extensions
    '(
        "bazaar"
        "bzr"
        "db_encryptable"
        "git"
        "svn"
        "tmp"))

(deffiletype-face-regexp omit3 "^  [d-].* \\.\\(.*$\\)" hidden)

(deffiletype-face "rich document" "DarkCyan" "document")

(deffiletype-face-components document
  :type-for-docstring "rich document"
  :extensions
  '(
     "CHM"
     "chm"
     "doc"
     "docx"
     "kdh"
     "odp"
     "ods"
     "odt"
     "otp"
     "ott"
     "pdf"
     "ppt"
     "pptx"
     "rtf"
     "sdw"
     "sdx"
     "shx"
     "sxc"
     "tex"
     "xls"
     "xlsx"))

(deffiletype-face "plain text" "DarkSeaGreen1" "plain")

(deffiletype-face-components plain :type-for-docstring "plain text"
  :extensions
  '(
     "CFG"
     "cfg"
     "cnf"
     "conf"
     "config"
     "default"
     "diff"
     "ebuild"
     "example"
     "inf"
     "INI"
     "ini"
     "log"
     "lrc"
     "m4"
     "org"
     "patch"
     "plist"
     "properties"
     "sample"
     "TXT"
     "Txt"
     "txt"))

(deffiletype-face "common" "Peru")

(deffiletype-face-components common
    :regexps
    '(
        "\\.keystore"
        "configure"
        "INSTALL.*"
        "Install.*"
        "CONTRIBUTING.*"
        "README.*"
        "readme.*"
        "todo"
        "Todo.*"
        "TODO.*"
        "Cask"
        "COPYING.*"
        "CHANGES"
        "Changes"
        "LICENSE"
        "ChangeLog"
        "Makefile"
        "Makefile\\.in"
        "MANIFEST\\.MF"
        "NOTICE\\.txt"
        "build\\.xml"
        "Manifest"
        "metadata\\.xml"
        "install-sh"
        "NEWS"
        "HACKING"
        "AUTHORS"
        "TAGS"
        "tag"
        "id_rsa"
        "id_rsa\\.pub"
        "id_dsa"
        "id_dsa\\.pub"
        "authorized_keys"
        "known_hosts"
        "CREDITS.*"
  )
)

(deffiletype-face "XML" "Chocolate")

(deffiletype-face-components XML
  :extensions
  '(
     "asp"
     "aspx"
     "dtd"
     "HTM"
     "htm"
     "HTML"
     "html"
     "js"
     "jsp"
     "jspx"
     "mht"
     "rng"
     "xaml"
     "XML"
     "xml"
     "xsd"
     "xsl"))

(deffiletype-face "compressed" "Orchid" "compress")

(deffiletype-face-components compress
  :type-for-docstring compressed
  :extensions
  '(
     "7Z"
     "7z"
     "apk"
     "bz2"
     "bzip2"
     "cab"
     "deb"
     "ear"
     "gpg"
     "gz"
     "gzip"
     "img"
     "iso"
     "jar"
     "lzma"
     "pkg"
     "RAR"
     "rar"
     "rpm"
     "tar"
     "taz"
     "tbz2"
     "tgz"
     "txz"
     "war"
     "wim"
     "XAR"
     "xar"
     "XZ"
     "xz"
     "Z"
     "z"
     "ZIP"
     "zip"))

(deffiletype-face "source code" "SpringGreen" "source")

(deffiletype-face-components source
  :type-for-docstring "source code"
  :extensions
  '(
     "a"
     "ahk"
     "asm"
     "C"
     "c"
     "cpp"
     "cs"
     "css"
     "ddl"
     "el"
     "erl"
     "go"
     "h"
     "hrl"
     "JAVA"
     "java"
     "lisp"
     "livecode"
     "lua"
     "p"
     "pas"
     "php"
     "pl"
     "py"
     "rb"
     "rev"
     "sch"
     "scheme"
     "scm"
     "sql"
     "st"))

(deffiletype-face "program" "blue")

(deffiletype-face-regexp program
  "^  -\\([r-][w-]-\\)\\{3\\}.*\\.\\(exe\\|EXE\\|bat\\|BAT\\|msi\\|MSI\\|\\(?:t?c\\|z\\)?sh\\|run\\|reg\\|REG\\|com\\|COM\\|vbx\\|VBX\\|bin\\|xpi\\|bundle\\|awk\\)$")

(deffiletype-face "executable" "green" "execute")

(deffiletype-face-regexp execute "^  -\\([r-][w-]-\\)\\{,2\\}[r-][w-]x"
 executable)

(deffiletype-face "music" "SteelBlue")

(deffiletype-face-components music
  :extensions
  '(
     "AAC"
     "aac"
     "FLAC"
     "flac"
     "m3u"
     "M4A"
     "m4a"
     "MID"
     "mid"
     "MP3"
     "mp3"
     "OGG"
     "ogg"
     "pls"
     "WAV"
     "wav"
     "WMA"
     "wma"))

(deffiletype-face "video" "SandyBrown")

(deffiletype-face-components video
  :extensions
  '(
     "3gp"
     "AVI"
     "avi"
     "divx"
     "f4v"
     "FLV"
     "flv"
     "m4v"
     "mkv"
     "mov"
     "mp4"
     "mpeg"
     "MPG"
     "mpg"
     "ogm"
     "ogv"
     "RM"
     "rm"
     "RMVB"
     "rmvb"
     "swf"
     "webm"
     "WMV"
     "wmv"
     "xvid"))

(deffiletype-face "image" "IndianRed2")

(deffiletype-face-components image
  :extensions
  '(
     "BMP"
     "bmp"
     "eps"
     "epsf"
     "GIF"
     "gif"
     "icns"
     "ico"
     "icon"
     "JPEG"
     "jpeg"
     "JPG"
     "jpg"
     "odg"
     "pcx"
     "pic"
     "pict"
     "PNG"
     "png"
     "svg"
     "tga"
     "tif"
     "tiff"
     "xbm"
     "xpm"))

(deffiletype-face
  "link"
  '((((class color) (background dark)) :foreground "yellow" :background "forest green") (t ())))

(deffiletype-face-regexp link
  "^  l\\|^  -.*\\.\\(lnk\\|LNK\\|desktop\\|torrent\\|url\\|URL\\)$")

;;; Custom ends here.

(defcustom dired-filetype-disabled-diredp-faces
  t
  "Turn off filetype matching from package `dired+', if installed.

Without this setting, some files will be highlighted by one
package and some by the other. Does not disable any other
features of package dired+; only dired+ file-type highlighting is
affected. If you're wondering why only some of the filetype faces
you define here are taking effect, and you have dired+ installed,
try this."
    :type 'boolean
    :tag "Disable dired+ filetype matching"
    :group 'dired-filetype-face)

(defvar dired-filetype-old-diredp-faces nil
  "Backup of filetype faces from package `dired+'.")

(defun dired-filetype-disable-diredp-faces-maybe ()
  "Turn off filetype matching from package `dired+' if present."
  (when (featurep 'dired+)
    (if dired-filetype-disabled-diredp-faces
      (when (bound-and-true-p diredp-font-lock-keywords-1)
        ;; then backup and clear diredp faces
        (setq dired-filetype-old-diredp-faces diredp-font-lock-keywords-1)
        (setq diredp-font-lock-keywords-1 nil))
      ;; else restore diredp faces
      (when (and
              dired-filetype-old-diredp-faces
              (boundp 'diredp-font-lock-keywords-1)
              (null diredp-font-lock-keywords-1))
        (setq diredp-font-lock-keywords-1 dired-filetype-old-diredp-faces)))))

(add-hook 'dired-filetype-setup-hook #'dired-filetype-disable-diredp-faces-maybe)

(defmacro deffiletype-setup (type &optional type-for-docstring type-for-symbol type-for-face)
    "Declare a function to tell dired how to display TYPE files.

If not nil, use TYPE-FOR-DOCSTRING instead of TYPE for
documentation.

If not nil, use TYPE-FOR-SYMBOL instead of TYPE to derive the
function symbol.

If not nil, use TYPE-FOR-FACE instead of TYPE to derive the
symbol for the associated face."
    (let*
        (
            (funcsym
                (i__d__f
                    "dired-filetype-set-%s-face"
                    (or type-for-symbol type)))
            (optsym (i__d__f "dired-filetype-%s-regexp" type))
            (docstring
                (format
                    "Set dired-filetype-face for %s files."
                    (or type-for-docstring type)))
            (facesym (i__d__f "dired-filetype-%s" (or type-for-face type))))
        `(progn
             (defun ,funcsym ()
                 ,docstring
                 (font-lock-add-keywords
                     nil
                     (list
                         (cons
                             (if (stringp ,optsym)
                                 ;; regexp
                                 ,optsym
                                 (let*
                                     (
                                         (regexps
                                             (awhen
                                                 (rest
                                                     (assq 'Regexps ,optsym))
                                                 (format
                                                     "\\(?:%s\\)"
                                                     (mapconcat #'identity it "\\|"))))
                                         (filenames
                                             (awhen
                                                 (rest
                                                     (assq 'Filenames ,optsym))
                                                 (format
                                                     "\\(?:%s\\)"
                                                     (regexp-opt it))))
                                         (extensions
                                             (awhen
                                                 (rest
                                                     (assq 'Extensions ,optsym))
                                                 (format
                                                     "\\(?:\\.%s\\)"
                                                     (regexp-opt it)))))
                                     (format
                                         "^  -.*\\(%s\\)$"
                                         (cl-reduce
                                             (lambda (x y)
                                                 (if x
                                                     (if y
                                                         (format "%s\\|%s" x y)
                                                         x)
                                                     y))
                                             (list regexps filenames extensions)))))
                             '((".+"
                                   (dired-move-to-filename)
                                   nil
                                   (0 (quote ,facesym))))))))
             (add-hook 'dired-filetype-setup-hook #',funcsym))))

(deffiletype-setup "document" "rich document")

(deffiletype-setup "plain" "plain text")

(deffiletype-setup "common")

(deffiletype-setup "XML")

(deffiletype-setup "compress" "compressed")

(deffiletype-setup "source" "source code")

(deffiletype-setup "omit1" "unimportant" "omit" "omit")

(deffiletype-setup "omit2" "backup and cache" nil "omit")

(deffiletype-setup "omit3" "hidden" nil "omit")

(deffiletype-setup "program")

(deffiletype-setup "execute" "executable")

(deffiletype-setup "music" "audio")

(deffiletype-setup "video")

(deffiletype-setup "image")

(deffiletype-setup "link")

;;;###autoload
(defun dired-filetype-setup()
  (run-hooks 'dired-filetype-setup-hook))

;; Append to mode hooks so ours are the last applied, overriding others.
;;;###autoload(add-hook 'dired-mode-hook 'dired-filetype-setup 'append)
(add-hook 'dired-mode-hook 'dired-filetype-setup 'append)
;;;###autoload(add-hook 'wdired-mode-hook 'dired-filetype-setup 'append)
(add-hook 'wdired-mode-hook 'dired-filetype-setup 'append)

(defadvice dired-toggle-read-only (after  dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(defadvice wdired-exit (after dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(defadvice wdired-finish-edit (after dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(defadvice wdired-abort-changes (after dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(provide 'dired-filetype-face)

;;; dired-filetype-face.el ends here.
