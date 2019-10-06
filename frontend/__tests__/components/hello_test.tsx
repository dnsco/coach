import { shallow } from "enzyme";
import React from "react";
import { Hello } from "../../src/components/hello";

describe("NiceCheckbox", () => {
  it("renders the checkbox with correct label", () => {
    expect(shallow(<Hello />).text()).toEqual("YO");
  });
});
