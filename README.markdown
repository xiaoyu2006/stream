# Stream

## Usage

```lisp
(ql:quickload :stream)
(use-package :stream)

;; list->stream converts a list to a stream
(list->stream '(1 2 3 4 5))

;; Create a stream of numbers from 1 to 5
(defparameter s (stream-enum-interval 1 5))

;; Use stream-reduce to sum the numbers in the stream
(stream-reduce #'+ 0 s) ; Returns 15

;; Use stream-reduce to multiply the numbers in the stream
(stream-reduce #'* 1 s) ; Returns 120
```

## Author

* Yi Cao ([me@ycao.top](mailto:me@ycao.top))

## Copyright

Copyright (c) 2023 Yi Cao ([me@ycao.top](mailto:me@ycao.top))

## License

Licensed under the [MIT License](https://opensource.org/licenses/MIT).
