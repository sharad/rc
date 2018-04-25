




;; https://notmuchmail.org/pipermail/notmuch/2012/012481.html
(if (>= emacs-major-version 24)
    (defadvice mm-shr (before load-gnus-arts activate)
      (require 'gnus-art nil t)
      (ad-disable-advice 'mm-shr 'before 'load-gnus-arts)))
