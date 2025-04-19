;;; ingredients.scm -- ASCII art for ingredients
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code ui ascii-art ingredients)
  #:export (ingredient->ascii))

;; Convert an ingredient to ASCII art
(define (ingredient->ascii ingredient)
  (let ((name (if (string? ingredient) 
                  ingredient 
                  (ingredient 'name))))
    (case (string->symbol name)
      ((butter)
       '("  ____________ "
         " /           /|"
         "/___________ / |"
         "|  BUTTER   |  |"
         "|           |  |"
         "|___________|/ "))
      
      ((herbs)
       '("    \\|/   "
         "    \\|/   "
         "  \\_\\|/_/ "
         "    \\|/   "
         "     |    "
         "     |    "))
      
      ((garlic)
       '("      __     "
         "    /   \\   "
         "   |     |  "
         "   \\     /  "
         "    \\___/   "
         "      |     "))
      
      ((salt)
       '("   _______   "
         "  /       \\  "
         " |  SALT   | "
         " |         | "
         " |_________| "
         "             "))
      
      ((onion)
       '("      __     "
         "    /    \\   "
         "   |      |  "
         "   |      |  "
         "    \\____/   "
         "             "))
      
      ((carrot)
       '("         ^     "
         "        / \\    "
         "       /   \\   "
         "      /     \\  "
         "     /       \\ "
         "    /_________\\"))
      
      ((celery)
       '("    |||    "
         "    |||    "
         "    |||    "
         "    |||    "
         "    |||    "
         "    |||    "))
      
      (else
       '("   _______   "
         "  /       \\  "
         " |         | "
         " |         | "
         " |_________| "
         "             ")))))
