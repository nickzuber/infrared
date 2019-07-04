import React from 'react';
import PropTypes from 'prop-types';
import {Utils} from './utils'
import * as Utils from './utils'
import Utils, {Other} from './utils'

class MyComponent extends React.Component {
  static propTypes = {
    children: PropTypes.node.isRequired
  };

  render () {
    return (
      <div>
        <button onClick={() => {}} />
      </div>
    );
  }
}

export default MyComponent;
