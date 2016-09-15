require('./node_modules/todomvc-common/base.js');
require('./node_modules/todomvc-common/base.css');
require('./node_modules/todomvc-app-css/index.css');

var React = require('react');
var ReactDOM = require('react-dom');

var Container = require('./src/Components/Container.purs').containerComponent;

ReactDOM.render(React.createElement(Container, {}, []), document.getElementById('app'));
