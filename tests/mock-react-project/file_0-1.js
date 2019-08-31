import React from 'react';
import PropTypes from 'prop-types';
import {Remote} from './source'
import * as Specifier from './source'
import Default, {Remote} from './source'

class MyComponent extends React.Component {
  static propTypes = {
    children: PropTypes.node.isRequired
  };

  some_literal = 42

  render () {
    return (
      <div>
        <button onClick={() => {}} />
      </div>
    );
  }
}

export function FunctionComponent (props) {
  const fnWillStillCauseRerender = () => {}
  return (
    <>
      <div onPress={fnWillStillCauseRerender} />
      <LocalFunctionComponent />
    </>
  );
}

const fnWillNotCauseRerender = () => {}
function LocalFunctionComponent (props) {
  return (
    <div onPress={fnWillNotCauseRerender} />
  );
}

export default MyComponent;
