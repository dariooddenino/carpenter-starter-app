require('./node_modules/todomvc-common/base.js');
require('./node_modules/todomvc-common/base.css');
require('./node_modules/todomvc-app-css/index.css');

var Main = require('./src/Main.purs');
Main["main"]();
