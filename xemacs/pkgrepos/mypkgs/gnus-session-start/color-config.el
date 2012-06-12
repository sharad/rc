;; (require 'color-theme)

;; (defun my-color-theme ()
;;   "Initialize faces according to my preferences."
;;   (interactive)
;;   (color-theme-install
;;    '(my-color-theme
;;      ;; frame parameters
;;      ;; ((background-color . "white")
;;      ;;  (background-mode . light)
;;      ;;  (border-color . "black")
;;      ;;  (cursor-color . "rgb:15/FF/00")
;;      ;;  (foreground-color . "black")
;;      ;;  (mouse-color . "black"))

;;      ;; ;; faces
;;      ;; (default ((t (nil))))
;;      ;; (bold ((t (:bold t :weight bold))))
;;      ;; (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
;;      ;; (italic ((t (:italic t :slant italic))))
;;      ;; (underline ((t (:underline t))))
;;      ;; (highlight ((t (:foreground "white" :background "rgb:31/6A/C5"))))

;;      ;; [...]

;;      ;; Gnus group buffer
;;      (gnus-group-mail-1-empty-face ((t (:foreground "rgb:50/50/B0"))))
;;      (gnus-group-mail-1-face ((t (:foreground "rgb:50/50/B0" :bold t))))
;;      (gnus-group-mail-2-empty-face ((t (:foreground "rgb:66/00/66"))))
;;      (gnus-group-mail-2-face ((t (:foreground "rgb:66/00/66" :bold t))))
;;      (gnus-group-mail-3-empty-face ((t (:foreground "rgb:00/77/77"))))
;;      (gnus-group-mail-3-face ((t (:foreground "rgb:00/77/77" :bold t))))

;;      (gnus-group-news-1-empty-face ((t (:foreground "rgb:50/50/B0"))))
;;      (gnus-group-news-1-face ((t (:foreground "rgb:50/50/B0" :bold t))))
;;      (gnus-group-news-2-empty-face ((t (:foreground "rgb:66/00/66"))))
;;      (gnus-group-news-2-face ((t (:foreground "rgb:66/00/66" :bold t))))
;;      (gnus-group-news-3-empty-face ((t (:foreground "rgb:00/77/77"))))
;;      (gnus-group-news-3-face ((t (:foreground "rgb:00/77/77" :bold t))))
;;      (gnus-group-news-4-empty-face ((t (:foreground "rgb:99/00/00"))))
;;      (gnus-group-news-4-face ((t (:foreground "rgb:99/00/00" :bold t))))
;;      (gnus-group-news-5-empty-face ((t (:foreground "rgb:00/00/99"))))
;;      (gnus-group-news-5-face ((t (:foreground "rgb:00/00/99" :bold t))))
;;      (gnus-group-news-6-empty-face ((t (:foreground "rgb:BB/66/00"))))
;;      (gnus-group-news-6-face ((t (:foreground "rgb:BB/66/00" :bold t))))

;;      ;; Gnus summary buffer
;;      (hl-line ((t (:background "SeaGreen1"))))
;;      (gnus-summary-selected-face ((t (:foreground "rgb:FF/66/33" :bold t))))
;;      (gnus-summary-high-unread-face ((t (:foreground "blue" :bold t))))
;;      (gnus-summary-high-read-face ((t (:foreground "rgb:80/00/80" :bold t))))
;;      (gnus-summary-high-ticked-face ((t (:foreground "hot pink" :bold t))))
;;      (gnus-summary-high-ancient-face ((t (:foreground "rgb:77/77/99" :bold t))))
;;      (gnus-summary-normal-unread-face ((t (:foreground "blue"))))
;;      (gnus-summary-normal-read-face ((t (:foreground "rgb:80/00/80"))))
;;      (gnus-summary-normal-ticked-face ((t (:foreground "hot pink"))))
;;      (gnus-summary-normal-ancient-face ((t (:foreground "rgb:77/77/99"))))
;;      (gnus-summary-low-unread-face ((t (:low t :foreground "blue" :italic t))))
;;      (gnus-summary-low-read-face ((t (:low t :foreground "rgb:80/00/80"
;;                                       :italic t))))
;;      (gnus-summary-low-ticked-face ((t (:low t :foreground "hot pink"
;;                                         :italic t))))
;;      (gnus-summary-low-ancient-face ((t (:low t :foreground "rgb:77/77/99"
;;                                          :italic t))))
;;      (gnus-summary-cancelled-face ((t (:italic t :foreground "gray55"
;;                                        :strike-through t))))

;;      ;; Gnus article buffer
;;      (gnus-header-name-face ((t (:foreground "rgb:33/99/CC" :bold t
;;                                  :family "Arial"))))
;;      (gnus-header-from-face ((t (:foreground "blue" :family "Arial"))))
;;      (gnus-header-subject-face ((t (:foreground "rgb:FF/66/33" :bold t))))
;;      (gnus-header-newsgroups-face ((t (:foreground "rgb:33/99/CC"
;;                                        :family "Arial"))))
;;      (gnus-header-content-face ((t (:foreground "rgb:33/99/CC"
;;                                     :family "Arial"))))
;;      (gnus-cite-attribution-face ((t (:foreground "rgb:50/50/B0"))))
;;      (gnus-cite-face-1 ((t (:foreground "rgb:50/50/B0"))))
;;      (gnus-cite-face-2 ((t (:foreground "rgb:66/00/66"))))
;;      (gnus-cite-face-3 ((t (:foreground "rgb:00/77/77"))))
;;      (gnus-cite-face-4 ((t (:foreground "rgb:99/00/00"))))
;;      (gnus-cite-face-5 ((t (:foreground "rgb:00/00/99"))))
;;      (gnus-cite-face-6 ((t (:foreground "rgb:BB/66/00"))))
;;      (gnus-cite-face-7 ((t (:foreground "rgb:50/50/B0"))))
;;      (gnus-cite-face-8 ((t (:foreground "rgb:66/00/66"))))
;;      (gnus-cite-face-9 ((t (:foreground "rgb:00/77/77"))))
;;      (gnus-cite-face-10 ((t (:foreground "rgb:99/00/00"))))
;;      (gnus-signature-face ((t (:foreground "rgb:7F/7F/7F"))))
;;      (gnus-emphasis-bold ((t (:bold t))))

;;      (widget-button-face ((t (:bold t :weight bold))))
;;      (widget-button-pressed-face ((t (:foreground "red"))))
;;      (widget-documentation-face ((t (:foreground "green4"))))
;;      (widget-field-face ((t (:background "gray85"))))
;;      (widget-inactive-face ((t (:foreground "dim gray"))))
;;      (widget-single-line-field-face ((t (:background "gray85"))))

;;      ;; when replying
;;      (message-header-name-face ((t (:foreground "rgb:33/99/CC" :bold t
;;                                     :family "Arial"))))
;;      (message-header-to-face ((t (:foreground "blue" :family "Arial"))))
;;      (message-header-cc-face ((t (:foreground "blue" :family "Arial"))))
;;      (message-header-subject-face ((t (:foreground "rgb:FF/66/33" :bold t))))
;;      (message-header-newsgroups-face ((t (:foreground "rgb:33/99/CC"
;;                                           :family "Arial"))))
;;      (message-header-xheader-face ((t (:foreground "red"))))
;;      (message-header-other-face ((t (:foreground "rgb:33/99/CC"
;;                                      :family "Arial"))))
;;      (message-separator-face ((t (:foreground "red" :bold t :family "Arial"))))
;;      (message-cited-text-face ((t (:foreground "rgb:50/50/B0"))))
;;      (message-mml-face ((t (:foreground "forest green"))))

;;      ;; Gnus/Message
;;      (gnus-emphasis-highlight-words ((t (:background "black"
;;                                          :foreground "yellow"))))
;;      (gnus-picon-face ((t (:background "white" :foreground "yellow"))))
;;      (gnus-picon-xbm-face ((t (:background "white" :foreground "yellow"))))
;;      (gnus-x-face ((t (:background "white" :foreground "black"))))

;;      ;; [...]

;;      )))

;; (provide 'my-color-theme)




(provide 'color-config)

