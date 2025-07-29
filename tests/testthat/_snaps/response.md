# response

    Code
      res
    Message
      An ambiorix response

# Response

    Code
      res
    Message
      
      -- A Response 

---

    Code
      print(robj(list(x = 1L)))
    Message
      i R object
    Output
      list(x = 1L)

---

    Code
      print(jobj(list(x = 1L)))
    Message
      i JSON object
    Output
      [1] "data"

---

    Code
      res
    Message
      
      -- A Response 
      
      -- Headers 
        * HEADER Content-Type
    Output
       chr "tab-separated-values"
    Message
        * HEADER Content-Length
    Output
       num 17645
    Message
        * HEADER xxx
    Output
       chr "hello"

