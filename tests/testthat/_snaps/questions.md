# choice and score validation gives actionable errors

    Code
      jev_choice("Choose", c(only = "one"))
    Condition
      Error:
      ! criteria must contain between 2 and 255 options.

---

    Code
      jev_choice("Choose", c("unnamed", "options"))
    Condition
      Error:
      ! criteria must have a non-empty name for every option.

---

    Code
      jev_score("Rate", "only one level")
    Condition
      Error:
      ! Score criteria must contain between 2 and 10 ordered levels.

# noul criteria must name both outcomes

    Code
      jev_noul("Is this true?", criteria = c(true = "Yes"))
    Condition
      Error:
      ! Noul criteria must contain exactly the names true and false.

