$(xml (:rss :version "2.0"
	    (:channel 
	     (:title "Example Blog")
	     (:link "http://example-blog-url.com/")
	     (:description "An example blog for site-generator")
	     (:lastBuildDate (str (build-time)))
	     (:language "en-us")
	     (loop for page in (get-pages "pages" :number 2)
		do (htm (:item
			 (:title (str (page-title page)))
			 (:link (str (page-address page)))
			 (:guid (str (page-address page)))
			 (:pubDate (str (page-date page :format +rfc+)))
			 (:description "<![CDATA[ "
				       (str (markup (get-content page :content)
						    :markup :markdown))
				      " ]]>")))))))
