
(in-package eshop-test)

(deftestsuite avail (eshop-avail-test)
  ((base-url  "http://localhost:8080/")
   (user "")
   (pass ""))
  (:function
   (verify-page (page)
                (trivial-timeout:with-timeout (1)
                  (ensure (eql 200
                               (second (multiple-value-list (drakma:http-request (concatenate 'string base-url page)
                                                                                 :basic-authorization (list user pass)))))))))
  (:tests
   (avail (verify-page ""))
   (cart (verify-page "cart"))
   (checkout (verify-page "checkout?button2"))
   (delivery (verify-page "delivery"))
   (warranty (verify-page "warranty"))
   (bonus (verify-page "bonus"))
   (corporate (verify-page "corporate"))
   (listservice (verify-page "listservice"))
   (faq (verify-page "faq"))
   (about (verify-page "about"))
   (levashovsky (verify-page "levashovsky"))
   (contacts (verify-page "contacts"))
   (vacancy (verify-page "vacancy"))
   (address (verify-page "address"))
   (warrantyservice (verify-page "warrantyservice"))
   (service (verify-page "service"))
   (dopolnitelnii-servis (verify-page "dopolnitelnii-servis"))
   (pricesc (verify-page "pricesc"))
   (articles/akcii (verify-page "articles/akcii"))
   (articles/news (verify-page "articles/news"))
   (articles/reviews (verify-page "articles/reviews"))
   (fcard (verify-page "fcard"))
   (noutbuki-i-komputery (verify-page "noutbuki-i-komputery"))
   (noutbuki (verify-page "noutbuki"))
   (product (verify-page "215543")) ; Product page
   ((verify-page "noutbuki?price-f=2000&price-t=40000&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1")) ; filter
   ((verify-page "noutbuki?price-f=1&price-t=2&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1")) ; zero-result filter
   ((verify-page "noutbuki?price-f=2000&price-t=40000&fullfilter=1")) ; filter
   ((verify-page "noutbuki?price-f=1&price-t=2&fullfilter=1")) ; zero-result filter
))
