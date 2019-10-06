/* eslint-env jest */
import { shallow } from "enzyme";
import "jest";
import React from "react";
import { Hello } from "../../src/components/hello";

describe("NiceCheckbox", () => {
  it("renders the checkbox with correct label", () => {
    expect(shallow(<Hello />).text()).toEqual("YO");
  });
});
