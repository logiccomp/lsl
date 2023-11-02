(use-modules (guix packages)
	     (guix git-download)
	     (gnu packages maths))

(define z3-4.8.8
  (package
   (inherit z3)
   (name "z3")
   (version "4.8.8")
   (home-page "https://github.com/Z3Prover/z3")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url home-page)
                                (commit (string-append "z3-" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1rn538ghqwxq0v8i6578j8mflk6fyv0cp4hjfqynzvinjbps56da"))))))

(concatenate-manifests
 (list
  (specifications->manifest
   '("racket"))
  (packages->manifest
   (list z3-4.8.8))))
