java-mode

(pkg "package " q ";")
(imp "import " q ";")

(sout "System.out.println(\"" q "\");")

;; (class "public class " p " {" n> q n "}")
(class "public class " (file-name-base (or (buffer-file-name) (buffer-name))) " {" n "    " q n "}")
(cl "public class " p " {" n "    " q n "}")
(psvm "public static void main(String[] args) {" n> q n> "}")

(if "if (" p ") {" n> q n> "}")
(elif "else if (" p ") {" n> q n> "}")
(else "else {" n> q n> "}")

(for "for (" p ") {" n> q n> "}")
