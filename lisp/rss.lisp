(in-package :pup)

(defun rss (category)
  (let ((cat-url (format nil "http://awarewolf.io/feed?cat=~A" category)))
    (cl-who:with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>")
      (:|rss| :|version| "2.0"
        :|xmlns:atom| "http://www.w3.org/2005/Atom"
       :|xmlns:content| "http://purl.org/rss/1.0/modules/content/"
        :|xmlns:dc| "http://purl.org/dc/elements/1.1/"
       :|xmlns:sy| "http://purl.org/rss/1.0/modules/syndication/"
        :|xmlns:wfw| "http://wellformedweb.org/CommentAPI/"
       :|xmlns:slash| "http://purl.org/rss/1.0/modules/slash/"
       (:|channel| (:|title| "Awarewolf/Seapup: an uncomfortable marriage in the head")
         (:|link| "https://awarewolf.io")
         (:|atom:link| :|href| cat-url :|rel| "self" :|type| "application/rss+xml")
         (:|description| "Musings of an aquatic shape-shifting cyborg canine")
         (:|sy:updatePeriod| "hourly")
         (:|sy:updateFrequency| "1")
         (:|image|
           (:|url| "http://awarewolf.io/static/img/favicons/favicon-196x196.png")
           (:|title| "Awarewolf/Seapup")
           (:|link| "https://awarewolf.io"))         
         (let* ((all-posts (get-posts-by-tag category))
                (posts (subseq all-posts 0 (min 15 (length all-posts)))))
           (loop for post in posts
                 do (cl-who:htm (:|item|
                                  (:|title| (cl-who:esc (post-title post)))
                                  (:|link| (cl-who:esc (blog-link post)))
                                  (:|dc:creator| (cl-who:str "<![CDATA[Ties Stuij]]>"))
                                  (:|slash:comments| (cl-who:str (length (post-comments post))))
                                  (loop for cat in (post-tags post)
                                        do (cl-who:htm
                                            (:|category|
                                              (cl-who:str (format nil "<![CDATA[~A]]>" cat)))))
                                  (:|guid| (cl-who:esc (blog-link post)))
                                  (:|description| (cl-who:str (format nil "<![CDATA[~A]]>" (get-summary post))))
                                  (:|content:encoded| (cl-who:str (format nil "<![CDATA[~A]]>" (get-body post))))
                                  (:|pubDate| (cl-who:str (format-timestring
                                                           nil
                                                           (get-post-date post)
                                                           :format  +rfc-1123-format+))))))))))))

