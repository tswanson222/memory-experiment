/**
 * jsPsych plugin for showing animations and recording keyboard responses
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 */

jsPsych.plugins.animation = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('animation', 'stimuli', 'image');

  plugin.info = {
    name: 'animation',
    description: '',
    parameters: {
	  words: {
		type: jsPsych.plugins.parameterType.STRING,
		pretty_name: 'Stimulus words',
		default: undefined,
		array: true,
		description: 'Stimulus words'
	  },
	  num_words: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Number of words',
		default: 6,
		description: 'Number of words used in task'
	  },
      time_per_word: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Frame time',
        default: 250,
        description: 'Duration to display each image.'
      },
	  first_slide_time: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Initial time',
		default: 250,
		description: 'Length of time that the empty slide, with no words, is shown.'
	  },
	  last_slide_time: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Last time',
		default: 250,
		description: 'Length of time the last slide, with all words, is shown.'
	  },
      prompt_location: {
        type: jsPsych.plugins.parameterType.SELECT,
        pretty_name: 'Prompt location',
        options: ['above','below'],
        default: 'below',
        description: 'Indicates whether to show prompt "above" or "below" the display area.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed below stimulus.'
      },
      sort_area_height: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Sort area height',
        default: 509,
        description: 'The height of the container that subjects can move the stimuli in.'
      },
      sort_area_width: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Sort area width',
        default: 1100,
        description: 'The width of the container that subjects can move the stimuli in.'
      },
      stim_height: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Stimulus height',
		default: 30,
		description: 'Height of stimulus words.'
	  },
	  stim_width: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Stimulus width',
		default: 90,
		description: 'Width of stimulus words.'
	  },
	  background: {
		type: jsPsych.plugins.parameterType.STRING,
		pretty_name: 'Background',
		default: '',
		description: 'Background image for words to appear on.'
	  },
	  blank_btn: {
		type: jsPsych.plugins.parameterType.OBJECT,
		pretty_name: 'Include blank button',
		default: true,
		description: 'Set as true to add blank button at bottom of screen'
	  },
	  display_boxes: {
		type: jsPsych.plugins.parameterType.OBJECT,
		pretty_name: 'Display 3 boxes',
		default: false,
		description: 'Use three-room setup for demonstration'
	  },
	  condition: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Experimental condition',
		default: undefined,
		description: 'Integer indicating experimental condition'
	  },
	  imageWidth: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Width of image',
		default: 715,
		description: 'Width of image'
	  },
	  imageHeight: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Height of image',
		default: 509,
		description: 'Height of image'
	  }
    }
  }

  plugin.trial = function(display_element, trial) {
	
	//if(trial.display_boxes == true && trial.num_words == 6){trial.num_words = 9};
	//if(trial.display_boxes == false && trial.num_words == 9){trial.num_words = 6};
	var whichCategory = []; for(var j = 0; j < trial.words.length; j++){whichCategory.push(j)};
	var rooms = [[0, 1, 2], [3, 4, 5], [6, 7, 8]];
	var interval_time = trial.first_slide_time;
    var animate_frame = -1;
    var animation_sequence = [];
    var time_sequence = [0];
    var startTime = (new Date()).getTime();
    //var current_stim = "";
    
	whichCategory = shuffleArray(whichCategory).splice(0, 3);
	var wordsABC = Array(3);
	for(var i = 0; i < 3; i++){
		wordsABC[i] = [];
		for(var j = 0; j < trial.words[whichCategory[i]].length; j++){wordsABC[i].push(j)};
		wordsABC[i] = shuffleArray(wordsABC[i]);
		wordsABC[i] = wordsABC[i].splice(0, (trial.num_words/3));
		rooms[i] = shuffleArray(rooms[i]);
	};
	rooms = shuffleArray(rooms);
	
	var catList = [];
	var wordList = [];
	var whichLocation = [];
	
	if(trial.condition == 1 || trial.condition == 4){
		for(var i = 0; i < 3; i++){repeat(function() {catList.push(whichCategory[i])}, (trial.num_words/3))};
		wordList = [].concat.apply([], wordsABC);
		if(trial.condition == 4){
			whichLocation = Array((trial.num_words/3));
			for(var i = 0; i < (trial.num_words/3); i++){
				whichLocation[i] = [];
				for(var j = 0; j < 3; j++){whichLocation[i].push(rooms[j][i])};
				whichLocation[i] = shuffleArray(whichLocation[i]);
			};
			whichLocation = [].concat.apply([], whichLocation);
		} else if(trial.condition == 1){
			whichLocation = [].concat.apply([], rooms);
		};
	} else if(trial.condition == 2 || trial.condition == 3 || trial.condition == 5){
		wordList = Array((trial.num_words/3));
		if(trial.condition == 3 || trial.condition == 5){whichLocation = Array((trial.num_words/3))};
		for(var i = 0; i < (trial.num_words/3); i++){
			wordList[i] = [];
			if(trial.condition == 3 || trial.condition == 5){whichLocation[i] = []};
			for(var j = 0; j < 3; j++){
				wordList[i].push(wordsABC[j][i]);
				if(trial.condition == 3 || trial.condition == 5){whichLocation[i].push(rooms[j][i])};
			};
		};
		repeat(function() {catList.push(whichCategory)}, (trial.num_words/3));
		var randomOrder = Array(3);
		if(trial.condition == 5){var disorder = Array(3)};
		for(var i = 0; i < 3; i++){
			randomOrder[i] = shuffleArray([0, 1, 2]);
			wordList[i] = randomOrder[i].map(j => wordList[i][j]);
			catList[i] = randomOrder[i].map(j => catList[i][j]);
			if(trial.condition == 3){whichLocation[i] = randomOrder[i].map(j => whichLocation[i][j])};
			if(trial.condition == 5){disorder[i] = shuffleArray([0, 1, 2])};
		};
		if(trial.condition == 5){
			let fullRandomOrder = [].concat.apply([], randomOrder);
			let fullDisorder = [].concat.apply([], disorder);
			let checkPairs = Array(9);
			for(var j = 0; j < 9; j++){checkPairs[j] = [fullRandomOrder[j], fullDisorder[j]].join('')};
			do {
				for(var i = 0; i < 3; i++){disorder[i] = shuffleArray([0, 1, 2])};
				fullDisorder = [].concat.apply([], disorder);
				for(var j = 0; j < 9; j++){checkPairs[j] = [fullRandomOrder[j], fullDisorder[j]].join('')};
			}
			while (hasDuplicates(checkPairs) == true);
			//while (pcorr(fullRandomOrder, fullDisorder) != 0);
			for(var i = 0; i < 3; i++){whichLocation[i] = disorder[i].map(j => whichLocation[i][j])};
		};
		wordList = [].concat.apply([], wordList);
		catList = [].concat.apply([], catList);
		if(trial.condition == 2){
			whichLocation = [].concat.apply([], rooms);
		} else if(trial.condition == 3 || trial.condition == 5){
			whichLocation = [].concat.apply([], whichLocation);
		};
	};
	
	if(trial.prompt !== null && trial.prompt_location == "above"){
	  display_element.innerHTML = '';
	  display_element.innerHTML += trial.prompt
	};
	
	display_element.innerHTML = '<div id="setup_arena" '+
      'style="position: relative; width:'+trial.sort_area_width+'px; height:'+trial.sort_area_height+'px; border:2px solid #444;"'+
      '><img src="'+trial.background+'" height='+trial.imageHeight+'px width='+trial.imageWidth+'px></img></div>';
		
	if(trial.prompt !== null && trial.prompt_location == "below"){display_element.innerHTML += trial.prompt};
    	
	if(trial.blank_btn !== null && trial.blank_btn !== false){
		if(trial.blank_btn == true){trial.blank_btn = 'Continue'};
		display_element.innerHTML += '<button class="jspsych-blank-btn">'+trial.blank_btn+'</button>';
	};
	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	if(trial.display_boxes == true){
		var top_locations = ["70", "42", "172", "25", "180", "100", "290", "450", "375"];
		var left_locations = ["254", "412", "323", "610", "800", "694", "510", "415", "580"];
		
		var bgimg = new Image();
		bgimg.src = trial.background;
		var bg_width = Number(bgimg.naturalWidth);
    
		display_element.querySelector("#setup_arena").innerHTML += '<div style="position: absolute; '+
		'width: '+(bg_width/2)+'px; height: '+(trial.sort_area_height/2)+'px; '+
		'left: '+((trial.sort_area_width - bg_width)/2)+'px; top: 0px; '+
		'background-color: rgba(200, 0, 0, .3)"></div>';
    
		display_element.querySelector("#setup_arena").innerHTML += '<div style="position: absolute; '+
		'width: '+(bg_width/2)+'px; height: '+(trial.sort_area_height/2)+'px; '+
		'left: '+(trial.sort_area_width/2)+'px; top: 0px; '+
		'background-color: rgba(0, 200, 0, .3)"></div>';
    
		display_element.querySelector("#setup_arena").innerHTML += '<div style="position: absolute; '+
		'width: '+(bg_width/2)+'px; height: '+(trial.sort_area_height/2)+'px; '+
		'left: '+((trial.sort_area_width/2) - (bg_width/4))+'px; top: '+(trial.sort_area_height/2)+'px; '+
		'background-color: rgba(0, 0, 200, .3)"></div>';
    } else {
		var top_locations = ["130", "60", "190", "235", "150", "145", "280", "380", "325"];
		var left_locations = ["254", "425", "375", "730", "800", "634", "410", "280", "524"];
		//var top_locations = ["70", "42", "172", "395", "272", "215"];
		//var left_locations = ["294", "777", "747", "694", "277", "510"];
	};
    /////////////////////////////////////////////////////////////////////////////////////////////////////
	
	var all_slides = function(){
		var showImage = true;
		animate_frame++;
		clearInterval(animate_interval);
		if(animate_frame < 0){
			interval_time = trial.first_slide_time;
		} else if(animate_frame == (trial.num_words - 1)){
			interval_time = trial.last_slide_time;
		} else if(animate_frame !== trial.num_words){
			interval_time = trial.time_per_word;
		} else if(animate_frame == trial.num_words){
			endTrial();
			showImage = false;
		}
		if(showImage){
			show_next_frame();
			animate_interval = setInterval(all_slides, interval_time);
		}
	}	
	
	var animate_interval = setInterval(all_slides, interval_time);

    function show_next_frame() {
	  
	  display_element.querySelector("#setup_arena").innerHTML += '<div id="jspsych-animation-image" class="wordStimuli" style="'+
	  'top: '+top_locations[whichLocation[animate_frame]]+'px; left: '+left_locations[whichLocation[animate_frame]]+'px; '+
	  'width: '+trial.stim_width+'px; height: '+trial.stim_height+'px">'+
	  '<pp>'+trial.words[catList[animate_frame]][wordList[animate_frame]]+'</pp></div>';

      //current_stim = trial.words[catList[animate_frame]][wordList[animate_frame]];
      
      animation_sequence.push({
        "cat": catList[animate_frame],
        "src": wordList[animate_frame],
        "loc": whichLocation[animate_frame]
      });
      
      time_sequence.push((new Date()).getTime() - startTime - sum(time_sequence));
    }

    function endTrial() {
	  
	  time_sequence = time_sequence.splice(1, (time_sequence.length - 1));
	  time_sequence.push((new Date()).getTime() - startTime - sum(time_sequence));
	  
      var trial_data = {
        "animation_sequence": JSON.stringify(animation_sequence),
        "condition": trial.condition,
        "rt": (new Date()).getTime() - startTime,
        "time_sequence": time_sequence
      };

      jsPsych.finishTrial(trial_data);
    }
  };
  
  // helper functions
  
  function shuffleArray(array) {
    for(var ii = array.length - 1; ii > 0; ii--){
      var jj = Math.floor(Math.random() * (ii + 1));
      var temp = array[ii];
      array[ii] = array[jj];
      array[jj] = temp;
    }
	return array
  };
  
  function repeat(func, times) {
    func();
    times && --times && repeat(func, times);
  };
  
  function reverseStr(str) {
    var newString = "";
    for(var i = str.length - 2; i > 0; i--){
      newString += str[i];
    };
    return newString;
  };
  
  function shortStr(str) {
    var newString = "";
    for(var i = 1; i < str.length - 1; i++){
      newString += str[i];
    };
    return newString;
  };
  
  const pcorr = (x, y) => {
    let sumX = 0,
      sumY = 0,
      sumXY = 0,
      sumX2 = 0,
      sumY2 = 0;
    const minLength = x.length = y.length = Math.min(x.length, y.length),
      reduce = (xi, idx) => {
        const yi = y[idx];
        sumX += xi;
        sumY += yi;
        sumXY += xi * yi;
        sumX2 += xi * xi;
        sumY2 += yi * yi;
      }
    x.forEach(reduce);
    return (minLength * sumXY - sumX * sumY) / Math.sqrt((minLength * sumX2 - sumX * sumX) * (minLength * sumY2 - sumY * sumY));
  };
  
  function hasDuplicates(array) {
    var valuesSoFar = Object.create(null);
    for(var i = 0; i < array.length; i++){
      var value = array[i];
      if(value in valuesSoFar){return true};
      valuesSoFar[value] = true;
    };
    return false;
  };
  
  function sum(array) {
    var total = 0;
    for(var i = 0; i < array.length; i++){total += Number(array[i])};
    return total;
  };

  return plugin;
})();
