open Jest;

describe("MySql2_id", () => {
  describe("toJson", () =>
    test("it should convert an id to a JSON string", () =>
      MySql2_id.fromJson(42.0 |. Js.Json.number)
      |. MySql2_id.toJson
      |. Js.Json.decodeString
      |. Belt.Option.getExn
      |> Expect.expect
      |> Expect.toBe("42")
    )
  );

  describe("fromJson", () => {
    test("should fail when given a JSON boolean", () => {
      let fn = () => Js.Json.boolean(true) |. MySql2_id.fromJson;

      Expect.expect(fn)
      |> Expect.toThrowMessage("unexpected_identifier_value");
    });

    test("should fail when given a JSON array", () => {
      let fn = () => Js.Json.array([||]) |. MySql2_id.fromJson;

      Expect.expect(fn)
      |> Expect.toThrowMessage("unexpected_identifier_value");
    });
  });
});
