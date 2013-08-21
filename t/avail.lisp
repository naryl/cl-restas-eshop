
(in-package eshop-test)

(declaim (optimize (debug 3) (safety 3)))

(deftestsuite avail ()
  ((base-url  "http://localhost:4246/")))

(addtest avail (verify-page base-url ""))
(addtest delivery (verify-page base-url "delivery"))
(addtest warranty (verify-page base-url "warranty"))
(addtest bonus (verify-page base-url "bonus"))
(addtest corporate (verify-page base-url "corporate"))
(addtest listservice (verify-page base-url "listservice"))
(addtest faq (verify-page base-url "faq"))
(addtest about (verify-page base-url "about"))
(addtest levashovsky (verify-page base-url "levashovsky"))
(addtest contacts (verify-page base-url "contacts"))
(addtest vacancy (verify-page base-url "vacancy"))
(addtest address (verify-page base-url "address"))
(addtest warrantyservice (verify-page base-url "warrantyservice"))
(addtest service (verify-page base-url "service"))
(addtest dopolnitelnii-servis (verify-page base-url "dopolnitelnii-servis"))
(addtest pricesc (verify-page base-url "pricesc"))
(addtest articles/akcii (verify-page base-url "articles/akcii"))
(addtest articles/news (verify-page base-url "articles/news"))
(addtest articles/reviews (verify-page base-url "articles/reviews"))
(addtest fcard (verify-page base-url "fcard"))
(addtest noutbuki-i-komputery (verify-page base-url "noutbuki-i-komputery"))
(addtest noutbuki (verify-page base-url "noutbuki"))
(addtest product (verify-page base-url "215543")) ; Product page
(addtest (verify-page base-url "noutbuki?price-f=2000&price-t=40000&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1")) ; filter
(addtest (verify-page base-url "noutbuki?price-f=1&price-t=2&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1")) ; zero-result filter
(addtest (verify-page base-url "noutbuki?price-f=2000&price-t=40000&fullfilter=1")) ; filter
(addtest (verify-page base-url "noutbuki?price-f=1&price-t=2&fullfilter=1")) ; zero-result filter

(defun verify-page (base-url page)
  (trivial-timeout:with-timeout (1)
    (ensure (eql 200
                 (second (multiple-value-list (drakma:http-request (concatenate 'string base-url page))))))))
