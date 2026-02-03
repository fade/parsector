;;; test-m3u.lisp - Tests for the M3U playlist parser example

(defpackage #:xyz.shunter.parsnip.test-m3u
  (:use #:cl #:parachute)
  (:local-nicknames (#:m3u #:xyz.shunter.parsnip.examples.m3u)))

(in-package #:xyz.shunter.parsnip.test-m3u)

(define-test m3u-parser
  :parent NIL)

(define-test simple-playlist
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:180,Test Song
/path/to/song.mp3
")))
    (is = 1 (length (m3u:m3u-playlist-tracks playlist)))
    (let ((track (first (m3u:m3u-playlist-tracks playlist))))
      (is = 180 (m3u:m3u-track-duration track))
      (is string= "Test Song" (m3u:m3u-track-title track))
      (is string= "/path/to/song.mp3" (m3u:m3u-track-path track)))))

(define-test playlist-metadata
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#PLAYLIST:My Favorite Songs
#EXTINF:120,Song One
/music/one.mp3
")))
    (is = 1 (length (m3u:m3u-playlist-metadata playlist)))
    (is equal '(:playlist . "My Favorite Songs")
        (first (m3u:m3u-playlist-metadata playlist)))))

(define-test expression-duration
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:(3*60)+45,Long Song
/path.mp3
")))
    (is = 225 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))))

(define-test negative-duration
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:-1,Unknown Duration
/path.mp3
")))
    (is = 1 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))))

(define-test multiple-tracks
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:180,Song A
/a.mp3
#EXTINF:240,Song B
/b.mp3
#EXTINF:300,Song C
/c.mp3
")))
    (is = 3 (length (m3u:m3u-playlist-tracks playlist)))
    (is = 180 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))
    (is = 240 (m3u:m3u-track-duration (second (m3u:m3u-playlist-tracks playlist))))
    (is = 300 (m3u:m3u-track-duration (third (m3u:m3u-playlist-tracks playlist))))))

(define-test complex-expression
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:(2+3)*60,Five Minutes
/path.mp3
")))
    (is = 300 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))))
