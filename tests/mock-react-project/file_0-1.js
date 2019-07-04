import React from 'react';
import PropTypes from 'prop-types';

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
