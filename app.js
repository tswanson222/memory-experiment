// LOADING MODULES
var express = require('express');
var mongoose = require('mongoose');
var body_parser = require('body-parser');

require('dotenv').config();


// INSTANTIATE APP
var app = express();

let uri = process.env.MONGODB_URI;


// MONGOOSE SETUP
mongoose.connect(uri);
var db = mongoose.connection;
db.on('error', console.error.bind(console, 'connection error'));
db.once('open', function callback(){
	console.log('database opened');
});

var emptySchema = new mongoose.Schema({}, {strict: false});
var Entry = mongoose.model('Entry', emptySchema);


// STATIC MIDDLEWARE
app.use(express.static(__dirname + '/public'));
app.use('/jsPsych', express.static(__dirname + '/jsPsych'));
app.use(body_parser.json({limit: '50mb'}));


// VIEW LOCATION, SET UP SERVING STATIC HTML
app.set('views', __dirname + '/public/views');
app.engine('html', require('ejs').renderFile);
app.set('view engine', 'html');


// ROUTING
app.get('/', function(request, response){
	response.render('experiment.html');
});

app.get('/finish', function(request, response){
	response.render('finish.html');
});

app.post('/experiment-data', function(request, response){
	Entry.create({
		"data":request.body
	});
	response.end();
});


// START THE SERVER
var server = app.listen(process.env.PORT, function(){
	console.log("Listening on port %d", server.address().port);
});
