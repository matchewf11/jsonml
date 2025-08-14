let get_num str i =
  let len = String.length str in
  if i >= len then Error "i out of bounds"
  else
    let num_len =
      match String.get str i with
      | '-' | '.' | '0' .. '9' ->
          let rec helper i =
            if i >= len then i
            else
              match String.get str i with
              | '.' | '0' .. '9' -> helper (i + 1)
              | _ -> i
          in
          helper (i + 1) - i
      | _ -> 0
    in
    if num_len = 0 then Error "no number found"
    else
      try Ok (float_of_string @@ String.sub str i num_len, num_len)
      with Failure _ -> Error "invalid number"

let get_str str i =
  let len = String.length str in
  if i >= len || String.get str i <> '"' then Error "not valid"
  else
    let rec helper j =
      if j >= len then Error "unterminated string"
      else match String.get str j with '"' -> Ok j | _ -> helper (j + 1)
    in
    match helper (i + 1) with
    | Error msg -> Error msg
    | Ok v ->
        let str_len = v - (i + 1) in
        Ok (String.sub str (i + 1) str_len, v - i + 1)
