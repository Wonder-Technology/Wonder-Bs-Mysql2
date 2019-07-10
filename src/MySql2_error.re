type t = {
  name: string,
  msg: string,
  code: string,
  errno: int,
  sql: option(string),
  sqlState: option(string),
  sqlMessage: option(string),
};

let make =
    (~name, ~msg, ~code, ~errno, ~sql=?, ~sqlState=?, ~sqlMessage=?, ()) => {
  name,
  msg,
  code,
  errno,
  sql,
  sqlState,
  sqlMessage,
};

let fromJs = json =>
  Json.Decode.{
    name: json |> withDefault("UNKNOWN", field("name", string)),
    msg: json |> withDefault("EMPTY_MESSAGE", field("message", string)),
    code: json |> withDefault("99999", field("code", string)),
    errno: json |> withDefault(99999, field("errno", int)),
    sql: json |> optional(field("sql", string)),
    sqlState: json |> optional(field("sqlState", string)),
    sqlMessage: json |> optional(field("sqlMessage", string)),
  };

let buildExtError = (name, msg, code, errno, sqlState, sqlMessage) =>
  switch (sqlState, sqlMessage) {
  | (Some(state), Some(message)) =>
    Failure({j|$name - $code ($errno) - $msg - ($state) $message|j})
  | (Some(state), None) =>
    Failure({j|$name - $code ($errno) - $msg - ($state)|j})
  | (None, Some(message)) =>
    Failure({j|$name - $code ($errno) - $msg - $message|j})
  | (None, None) => Failure({j|$name - $code ($errno) - $msg|j})
  };

let sqlErrorToExn = t => {
  let {name, msg, code, errno, sql, sqlState, sqlMessage} = t;

  buildExtError(name, msg, code, errno, sqlState, sqlMessage);
};

external convertToJsObj: t => Js.t({..}) = "%identity";

let transationErrorToExn = t => {
  let tObj = convertToJsObj(t);

  let name = tObj##name;
  let msg = tObj##msg;
  let code = tObj##code;
  let errno = tObj##errono;
  let sqlState = tObj##sqlState;
  let sqlMessage = tObj##sqlMessage;

  buildExtError(name, msg, code, errno, sqlState, sqlMessage);
};

let fromJsToExn = json => json |> fromJs |> sqlErrorToExn;

let invalidResponseType = () =>
  make(
    ~name="InvalidResponseType",
    ~msg="invalid_driver_result",
    ~code="UNKNOWN_RESPONSE_TYPE",
    ~errno=99999,
    (),
  );