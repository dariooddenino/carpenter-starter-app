var React = require('react');
var ReactDOM = require('react-dom');

var Container = require('./src/Components/Container.purs').containerComponent;

ReactDOM.render(React.createElement(Container, {}, []), document.getElementById('app'));
