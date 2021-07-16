/**
 * jspsych-free-sort
 * plugin for drag-and-drop sorting of a collection of images
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 */


jsPsych.plugins['free-sort'] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('free-sort', 'stimuli', 'image');

  plugin.info = {
    name: 'free-sort',
    description: '',
    parameters: {
      words: {
		type: jsPsych.plugins.parameterType.STRING,
		pretty_name: 'Word stimuli',
		default: undefined,
		array: true,
		description: 'Words that were imported from text file'
	  },
	  num_words: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Number of words',
		default: 6,
		description: 'Number of words used in task'
	  },
	  correctAnswers: {
		type: jsPsych.plugins.parameterType.OBJECT,
		pretty_name: 'Preceding setup sort',
		default: undefined,
		array: true,
		description: 'A JSON string of the preceding setup trial'
	  },
      stim_height: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus height',
        default: 30,
        description: 'Height of images in pixels.'
      },
      stim_width: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus width',
        default: 90,
        description: 'Width of images in pixels'
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
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'It can be used to provide a reminder about the action the subject is supposed to take.'
      },
      prompt_location: {
        type: jsPsych.plugins.parameterType.SELECT,
        pretty_name: 'Prompt location',
        options: ['above','below'],
        default: 'below',
        description: 'Indicates whether to show prompt "above" or "below" the sorting area.'
      },
      button_label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button label',
        default:  'Continue',
        description: 'The text that appears on the button to continue to the next trial.'
      },
      disable_button: {
		type: jsPsych.plugins.parameterType.OBJECT,
		pretty_name: 'Disable done button',
		default: false,
		description: 'Prevent from continuing until all words are sorted'
	  },
      background: {
		type: jsPsych.plugins.parameterType.STRING,
		pretty_name: 'Background',
		default: null,
		description: 'Background image for task.'
	  },
	  no_margin: {
		type: jsPsych.plugins.parameterType.OBJECT,
		pretty_name: 'No margin above arena',
		default: true,
		description: 'Make true if a blank button is used in setup'
	  },
	  display_boxes: {
		type: jsPsych.plugins.parameterType.OBJECT,
		pretty_name: 'Display 3 boxes',
		default: false,
		description: 'Use three-room setup for demonstration'
	  },
	  imageWidth: {
		type: jsPsych.plugins.parameterType.INT,
		pretty_name: 'Width of image',
		default: 715,
		description: 'Width of actual image'
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
    var start_time = (new Date()).getTime();
    
    var selectedWords = [];
    var selectedCats = [];
    var correctLocations = [[[[]]]];
    for(var i = 0; i < trial.correctAnswers.length; i++){
		correctLocations[i] = [trial.correctAnswers[i].cat, trial.correctAnswers[i].src, trial.correctAnswers[i].loc, i];
		selectedCats[i] = trial.correctAnswers[i].cat;
		selectedWords[i] = trial.correctAnswers[i].src;
	};
	correctLocations.sort(sortByLoc);
    
    var html = "";
    // check if there is a prompt and if it is shown above
    if(trial.prompt !== null && trial.prompt_location == "above"){html += trial.prompt};

    html += '<div '+
      'id="jspsych-free-sort-arena" '+
      'class="jspsych-free-sort-arena" '+
      'style="position: relative; '+
      'width:'+trial.sort_area_width+'px; height:'+trial.sort_area_height+'px; '+
      'border:2px solid #444; '+
      '"><img src="'+trial.background+'" height='+trial.imageHeight+'px width='+trial.imageWidth+'px>'+
      '</img></div>';

    // check if prompt exists and if it is shown below
    if(trial.prompt !== null && trial.prompt_location == "below"){html += trial.prompt};

    display_element.innerHTML = html;
    
    if(trial.no_margin == false){display_element.querySelector("#jspsych-free-sort-arena").style['margin-top'] = '34px'};
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////
	if(trial.display_boxes == true){
		var top_locations = ["70", "42", "172", "25", "180", "100", "290", "450", "375"];
		var left_locations = ["254", "412", "323", "610", "800", "694", "510", "415", "580"];
		
		var bgimg = new Image();
		bgimg.src = trial.background;
		var bg_width = Number(bgimg.naturalWidth);
    
		display_element.querySelector("#jspsych-free-sort-arena").innerHTML += '<div style="position: absolute; '+
		'width: '+(bg_width/2)+'px; height: '+(trial.sort_area_height/2)+'px; '+
		'left: '+((trial.sort_area_width - bg_width)/2)+'px; top: 0px; '+
		'background-color: rgba(200, 0, 0, .3)"></div>';
    
		display_element.querySelector("#jspsych-free-sort-arena").innerHTML += '<div style="position: absolute; '+
		'width: '+(bg_width/2)+'px; height: '+(trial.sort_area_height/2)+'px; '+
		'left: '+(trial.sort_area_width/2)+'px; top: 0px; '+
		'background-color: rgba(0, 200, 0, .3)"></div>';
    
		display_element.querySelector("#jspsych-free-sort-arena").innerHTML += '<div style="position: absolute; '+
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

    // store initial location data
    var init_locations = [];
    var containers = [];
    var wordStimuli = [];
    var coords = [];
    
    for(var i = 0; i < trial.num_words; i++){
		coords.push(random_coordinate(trial.sort_area_width, trial.stim_width, trial.sort_area_height, trial.stim_height, trial.imageWidth, trial.num_words, i));
	};
	
	coords = shuffleArray(coords);

    for (var i = 0; i < trial.num_words; i++) {      
	  containers[i] = '<div class="free-sort-droppable" '+
	    'data-loc="'+i+'" '+
	    'style="width: '+trial.stim_width+'px; height: '+trial.stim_height+'px; '+
	    'top:'+top_locations[i]+'px; left:'+left_locations[i]+'px"><pp> </pp></div>';
	   
	  wordStimuli[i] = '<div class="free-sort-draggable" '+
	    'data-cat="'+selectedCats[i]+'" '+
	    'data-src="'+selectedWords[i]+'" '+
	    'draggable="false" '+
	    'style="cursor: move; '+
	    'width: '+trial.stim_width+'px; height: '+trial.stim_height+'px; '+
	    'top: '+coords[i].y+'px; left: '+coords[i].x+'px">'+
	    '<pp>'+trial.words[selectedCats[i]][selectedWords[i]]+'</pp></div>';
	  
	  display_element.querySelector("#jspsych-free-sort-arena").innerHTML += containers[i];
	  display_element.querySelector("#jspsych-free-sort-arena").innerHTML += wordStimuli[i];

      init_locations.push({
		"cat": selectedCats[i],
		"src": selectedWords[i],
        "x": coords[i].x,
        "y": coords[i].y
      });
    }

    display_element.innerHTML += '<button id="free-sort-done-btn" class="jspsych-btn">'+trial.button_label+'</button>';
    var doneButton = display_element.querySelector("#free-sort-done-btn");
    if(trial.disable_button == true){doneButton.setAttribute('disabled', 'disabled')};

    var maxz = 1;

    var moves = [];
    
    var final_locations = [];
    
    var num_complete = 0;

    var draggables = display_element.querySelectorAll('.free-sort-draggable');
    
	for(var i = 0; i < draggables.length; i++){
      draggables[i].addEventListener('mousedown', function(event){
        var x = event.pageX - event.currentTarget.offsetLeft;
        var y = event.pageY - event.currentTarget.offsetTop - window.scrollY;
        var elem = event.currentTarget;
        elem.style.zIndex = ++maxz;

        var mousemoveevent = function(e){
          elem.style.top =  Math.min(trial.sort_area_height - trial.stim_height, Math.max(0,(e.clientY - y))) + 'px';
          elem.style.left = Math.min(trial.sort_area_width  - trial.stim_width,  Math.max(0,(e.clientX - x))) + 'px';
        }
        document.addEventListener('mousemove', mousemoveevent);

        var mouseupevent = function(e){
          document.removeEventListener('mousemove', mousemoveevent);
          moves.push({
			"cat": elem.dataset.cat,
            "src": elem.dataset.src,
            "x": elem.offsetLeft,
            "y": elem.offsetTop
          });
          document.removeEventListener('mouseup', mouseupevent);
        }
        document.addEventListener('mouseup', mouseupevent);
      });
    }
    
    var destroy = true;
    
    $(function(){
		$(".free-sort-draggable").draggable({
			cursor: 'move',
			revert: "invalid"
		});
		$(".free-sort-droppable").droppable({
			hoverClass: 'hover',
			drop: function(event,ui){
				var catID = Number(ui.draggable.data('cat'));
				var wordID = Number(ui.draggable.data('src'));
				var locID = $(this).data('loc');
				$(this)
				.addClass("active");
				if(destroy == true){
					$(this).html(ui.draggable.remove().html());
					$(this).droppable('destroy');
					$(this)
					.addClass("active")
					.find("pp")
					.html(trial.words[catID][wordID]);
			    } else {
					ui.draggable.position({of: $(this), my: 'left top', at: 'left top'});
					ui.draggable.draggable('option', 'revert', false);
				};
				final_locations.push({
					"cat": catID,
					"src": wordID,
					"loc": locID
				});
				num_complete++;
				if(num_complete == trial.num_words && trial.disable_button == true){
					doneButton.removeAttribute('disabled');
				};
			}
		});
	});
	
    display_element.querySelector('#free-sort-done-btn').addEventListener('click', function(){

      var end_time = (new Date()).getTime();
      var rt = end_time - start_time;
      // gather data
      // get final position of all objects
        //var final_locations = [];
        //var matches = display_element.querySelectorAll('.jspsych-free-sort-draggable');
        //for(var i=0; i < matches.length; i++){
        //  final_locations.push({
        //    "src": (i + 1),
        //    //"src": matches[i].dataset.src,
        //    "x": parseInt(matches[i].style.left),
        //    "y": parseInt(matches[i].style.top)
        //  });
        //}
      
      var finalLocations = [[[[]]]];
      for(var i = 0; i < final_locations.length; i++){
		  finalLocations[i] = [final_locations[i].cat, final_locations[i].src, final_locations[i].loc, i];
	  };
	  finalLocations.sort(sortByLoc);
	  
	  var totalCorrect = 0;
	  for(var i = 0; i < finalLocations.length; i++){
		  if(finalLocations[i][0] == correctLocations[i][0] && finalLocations[i][1] == correctLocations[i][1]){
			  totalCorrect++
		  }
	  };

      var trial_data = {
        "init_locations": JSON.stringify(init_locations),
        "moves": JSON.stringify(moves),
        "final_locations": JSON.stringify(final_locations),
        "rt": rt,
        "sort_correct": totalCorrect,
        "finalLocations": finalLocations
      };
      
      if(trial.disable_button == false){trial_data["completed"] = num_complete};

      // advance to next part
      display_element.innerHTML = '';
      jsPsych.finishTrial(trial_data);
    });

  };

  // helper functions

  function random_coordinate(max_width, word_width, max_height, word_height, background, numWords, placement) {
	//var bg = new Image();
	//bg.src = background;
	//var background_width = Number(bg.naturalWidth);
	var background_width = background;
	var maxWidth = (max_width/2) - (background_width/2) - word_width;
	var sectionSize = (max_height - 1)/numWords;
	var maxY = (sectionSize * (placement + 1)) - word_height;
	var minY = sectionSize * placement;
	var rnd_x = Math.floor(Math.random() * (maxWidth - 1));
    var rnd_y = Math.floor(Math.random() * (maxY - minY) + minY);
    return {
      x: rnd_x,
      y: rnd_y
    };
  }
  
  function sortByLoc(a, b){
	  if(a[2] === b[2]){
		  return 0;
	  } else {
		  return (a[2] < b[2]) ? -1 : 1;
	  }
  };
  
  function shuffleArray(array) {
	  for(var ii = array.length - 1; ii > 0; ii--){
		  var jj = Math.floor(Math.random() * (ii + 1));
		  var temp = array[ii];
		  array[ii] = array[jj];
		  array[jj] = temp;
	  }
	  return array
  };

  return plugin;
})();
