# jev_ask validates questions before authenticating

    Code
      jev_ask("state", questions = list(bad = "not a question"))
    Condition
      Error:
      ! Question bad is not a jev_choice(), jev_score(), or jev_noul() object.

