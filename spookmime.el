;;; spookmime.el --- Echelon fodder mime boundary generators

;; Copyright (C) 1997, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: fnord
;; Created: 1997-02-06

;; $Id: spookmime.el,v 1.2 2002/07/16 07:30:53 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Original concept by Bryan O'Sullivan <bos@serpentine.com>.

;; The function spookmime-make-mime-boundary returns a string which is
;; suitable for use as a MIME multipart separator.  MUAs and news readers
;; need to be instrumented to use this routine instead of whatever default
;; they normally provide.  VM is already so instrumented at the end of this
;; package.

;; Updates of this program may be available via the URL
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

;; XEmacs as of version 21.1 does not provide 'spook
(or (fboundp 'snarf-spooks)
    (load "spook"))

(defvar spookmime-boundary-chars
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Valid mime boundary characters")

(defvar spookmime-min-boundary-length 40
  "*Minimum length of spook phrases in mime boundary.
Generated mime boundaries will contain enough spook phrases to make the
entire string at least this many characters long.")

(defvar spookmime-min-phrase-count    3
  "*Minimum number of spook phrases to put in mime boundary.
Generated mime boundaries will always contain at least this many spook
phrases even if the combined length exceeds `spookmime-min-boundary-length'.")

(defvar spookmime-spook-phrases       (snarf-spooks))
(defvar spookmime-separator-chars     [?- ?/])
(defvar spookmime-illegal-re          "[^ A-Za-z0-9/+_-]+\\|\\([ \t]+$\\)")
(defvar spookmime-substitute-re       "[ _]")
(defvar spookmime-substitute-char     ?-)

;;;###autoload
(defun spookmime-make-mime-boundary ()
  "Return a mime boundary with NSA fodder phrases."
  (let ((spook-separator (char-to-string (aref spookmime-separator-chars 0)))
        (alt-separator nil)
        (spook-list nil)
        (boundary-length 0)
        (spook-count spookmime-min-phrase-count))

    ;; Generate list of spook phrases to put in string.
    ;; Make sure there are no repeats.
    (while (or (> spook-count 0)
               (< boundary-length spookmime-min-boundary-length))
      (let ((new (aref spookmime-spook-phrases
                       (spookmime-random (length spookmime-spook-phrases)))))
        (cond ((memq new spook-list))
              (t
               (setq spook-list (cons new spook-list))
               (setq boundary-length (+ boundary-length (length new)))
               (setq spook-count (1- spook-count))))))

    (let ((nlist spook-list)
          p s)
      (while nlist
        (setq p 0
              s (car nlist))
        ;; Eliminate any illegal characters from chosen strings.
        (while (string-match spookmime-illegal-re s p)
          (setq p (+ p (- (match-end 0) (match-beginning 0)))
                s (concat (substring s 0 (match-beginning 0))
                          (substring s (match-end 0)))))
        ;; Make substitutions in remaining string
        (setq p 0)
        (while (string-match spookmime-substitute-re s p)
          ;; If we're going to modify a string in-place, make sure it is a
          ;; copy so as not to corrupt the original spooks table.
          (and (eq s (car nlist))
               (setq s (copy-sequence s)))
          (aset s (match-beginning 0) spookmime-substitute-char)
          (setq p (match-end 0)))
        (cond ((eq spook-separator alt-separator))
              ((string-match spook-separator s)
               (setq alt-separator
                     (char-to-string (aref spookmime-separator-chars 1)))
               (setq spook-separator alt-separator)))
        (setcar nlist s)
        (setq nlist (cdr nlist))))

    (concat (mapconcat 'identity spook-list spook-separator)
            spook-separator
            (spookmime-make-random-mime-boundary))))

;; Return a dull mime boundary with random characters.
(defun spookmime-make-random-mime-boundary (&optional length)
  (let ((boundary (make-string (or length 10) ?a))
	(i 0))
    (while (< i (length boundary))
      (aset boundary i
            (aref spookmime-boundary-chars
                  (spookmime-random (length spookmime-boundary-chars))))
      (setq i (1+ i)))
    boundary))

(defun spookmime-random (&optional n)
  (if n
      (if (string-lessp "19" emacs-version)
          (random n)
        (% (abs (lsh (random) -8)) n))
    (abs (lsh (random) -8))))


;; VM glue.  There is no hook, so we have to modify the standard routine.
(defadvice vm-mime-make-multipart-boundary (around spookmime activate)
  (setq ad-return-value (spookmime-make-mime-boundary)))

;; GNUS glue.  To use this, mml-boundary-function to this function.
(defun spookmime-mml-make-boundary (&optional this-msg-count)
  (spookmime-make-mime-boundary))

(provide 'spookmime)

;;; spookmime.el ends here.
