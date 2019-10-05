/* eslint-env jest */
import React from 'react'
import {shallow} from 'enzyme'
import {Hello} from "../../src/components/hello";

describe('NiceCheckbox', () => {
    it('renders the checkbox with correct label', () => {
        expect(
            shallow(
                <Hello wat="no"/>
            ).text()
        ).toEqual('YO')
    })
})
