
(in-package eshop-test)

(deftestsuite avail (eshop-avail-test)
  ((base-url  "http://localhost:8080/")
   (user "")
   (pass ""))
  :setup (log:info base-url user pass))

(addtest avail (verify-page base-url "" user pass))
(addtest cart (verify-page base-url "cart" user pass))
(addtest checkout (verify-page base-url "checkout?button2" user pass))
(addtest delivery (verify-page base-url "delivery" user pass))
(addtest warranty (verify-page base-url "warranty" user pass))
(addtest bonus (verify-page base-url "bonus" user pass))
(addtest corporate (verify-page base-url "corporate" user pass))
(addtest listservice (verify-page base-url "listservice" user pass))
(addtest faq (verify-page base-url "faq" user pass))
(addtest about (verify-page base-url "about" user pass))
(addtest levashovsky (verify-page base-url "levashovsky" user pass))
(addtest contacts (verify-page base-url "contacts" user pass))
(addtest vacancy (verify-page base-url "vacancy" user pass))
(addtest address (verify-page base-url "address" user pass))
(addtest warrantyservice (verify-page base-url "warrantyservice" user pass))
(addtest service (verify-page base-url "service" user pass))
(addtest dopolnitelnii-servis (verify-page base-url "dopolnitelnii-servis" user pass))
(addtest pricesc (verify-page base-url "pricesc" user pass))
(addtest articles/akcii (verify-page base-url "articles/akcii" user pass))
(addtest articles/news (verify-page base-url "articles/news" user pass))
(addtest articles/reviews (verify-page base-url "articles/reviews" user pass))
(addtest fcard (verify-page base-url "fcard" user pass))
(addtest noutbuki-i-komputery (verify-page base-url "noutbuki-i-komputery" user pass))
(addtest noutbuki (verify-page base-url "noutbuki" user pass))
(addtest product (verify-page base-url "215543" user pass)) ; Product page
(addtest (verify-page base-url "noutbuki?price-f=2000&price-t=40000&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1" user pass)) ; filter
(addtest (verify-page base-url "noutbuki?price-f=1&price-t=2&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1" user pass)) ; zero-result filter
(addtest (verify-page base-url "noutbuki?price-f=2000&price-t=40000&fullfilter=1" user pass)) ; filter
(addtest (verify-page base-url "noutbuki?price-f=1&price-t=2&fullfilter=1" user pass)) ; zero-result filter

(defun verify-page (base-url page &optional user pass)
  (trivial-timeout:with-timeout (1)
    (ensure (eql 200
                 (second (multiple-value-list (drakma:http-request (concatenate 'string base-url page)
                                                                   :basic-authorization (list user pass))))))))
