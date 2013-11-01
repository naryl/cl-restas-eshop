
(in-package eshop-test)

(deftestsuite let+ (eshop-test)
  ()
  (:tests
   (basic
    (ensure
     (eql 42
          (let+ () 42)))
    (ensure
     (eql nil
          (let+ (var) var)))
    (ensure
     (equal (list nil 43)
          (let+ ((var)
                 (v1 42)
                 (v2 (1+ v1)))
            (list var v2))))
    (ensure
     (equal (list 1 2 3)
            (let+ (((a b c) (list 1 2 3)))
              (list a b c))))
    (ensure
     (equal (list 1 2 3)
            (let+ (((:values a b c) (values 1 2 3)))
              (list a b c))))
    (ensure
     (equal (list 2 3)
            (let+ (((_ b _c) (list 1 2 3)))
              (list b _c)))))))
