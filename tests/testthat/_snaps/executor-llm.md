# backend map handles empty and invalid inputs without constructing chats

    Code
      jev_map("A", questions, backend = backend, concurrency = 2)
    Condition
      Error:
      ! The ellmer backend supports only concurrency = 1.

---

    Code
      jev_map("A", questions, backend = backend, retry_budget = 120)
    Condition
      Error:
      ! The ellmer backend cannot apply retry_budget; retries belong to the transport.

---

    Code
      jev_map("A", questions, backend = backend, max_retries = 2)
    Condition
      Error:
      ! The ellmer backend requires max_retries = 0; retries belong to the transport.

