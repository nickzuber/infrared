import React from 'react';
import PropTypes from 'prop-types';
import {Remote} from './source'
import * as Specifier from './source'
import Default, {Remote} from './source'

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
