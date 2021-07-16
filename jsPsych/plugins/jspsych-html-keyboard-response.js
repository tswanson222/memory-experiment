/**
 * jspsych-html-keyboard-response
 * Josh de Leeuw
 *
 * plugin for displaying a stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/


jsPsych.plugins["html-keyboard-response"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'html-keyboard-response',
    description: '',
    parameters: {
      stimulus: {
        type: jsPsych.plugins.parameterType.HTML_STRING,
        pretty_name: 'Stimulus',
        default: undefined,
        description: 'The HTML string to be displayed'
      },
      choices: {
        type: jsPsych.plugins.parameterType.KEYCODE,
        array: true,
        pretty_name: 'Choices',
        default: jsPsych.ALL_KEYS,
        description: 'The keys the subject is allowed to press to respond to the stimulus.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed below the stimulus.'
      },
      prompt_location: {
        type: jsPsych.plugins.parameterType.SELECT,
        pretty_name: 'Prompt location',
        options: ['above','below'],
        default: 'below',
        description: 'Indicates whether to show prompt "above" or "below" the sorting area.'
      },
      stimulus_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus duration',
        default: null,
        description: 'How long to hide the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show trial before it ends.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, trial will end when subject makes a response.'
      },
      countdown: {
		type: jsPsych.plugins.parameterType.BOOL,
		pretty_name: 'Countdown timer',
		default: false,
		description: 'Displays countdown timer based on trial_duration'
	  }
    }
  }

  plugin.trial = function(display_element, trial) {
	  
	var new_html = "";
	if(trial.prompt !== null && trial.prompt_location == "above"){new_html += trial.prompt}
	if(trial.countdown == true){
		new_html += '<div style="font-size:36px" id="jspsych-html-keyboard-response-stimulus"><strong>'+trial.stimulus+'</strong></div>';
	} else {
		new_html += '<div id="jspsych-html-keyboard-response-stimulus">'+trial.stimulus+'</div>';
	}
    if(trial.prompt !== null && trial.prompt_location == "below"){new_html += trial.prompt}
    display_element.innerHTML = new_html;
    
    if(trial.countdown == true){
		if(trial.stimulus > 0){
			trial.ticker = setInterval(function(){
				trial.stimulus--;
				if(trial.stimulus <= 1){
					clearInterval(trial.ticker);
					trial.stimulus = 1;
				}
				var secs = trial.stimulus;
				var mins = Math.floor(secs / 60);
				secs -= mins * 60;
				trial.stimulus = secs;
				new_html = "";
				if(trial.prompt !== null && trial.prompt_location == "above"){new_html += trial.prompt;};
				new_html += '<div style="font-size:36px" id="jspsych-html-keyboard-response-stimulus"><strong>'+trial.stimulus+'</strong></div>';
				if(trial.prompt !== null && trial.prompt_location == "below"){new_html += trial.prompt;};
				display_element.innerHTML = new_html;
			}, 1000);
		}
	} 

    // store response
    var response = {
      rt: null,
      key: null
    };

    // function to end trial when it is time
    var end_trial = function() {

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // kill keyboard listeners
      if (typeof keyboardListener !== 'undefined') {
        jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);
      }

      // gather the data to store for the trial
      var trial_data = {
        "rt": response.rt,
        "stimulus": trial.stimulus,
        "key_press": response.key
      };

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };

    // function to handle responses by the subject
    var after_response = function(info) {

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector('#jspsych-html-keyboard-response-stimulus').className += ' responded';

      // only record the first response
      if (response.key == null) {
        response = info;
      }

      if (trial.response_ends_trial) {
        end_trial();
      }
    };

    // start the response listener
    if (trial.choices != jsPsych.NO_KEYS) {
      var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
        callback_function: after_response,
        valid_responses: trial.choices,
        rt_method: 'date',
        persist: false,
        allow_held_key: false
      });
    }

    // hide stimulus if stimulus_duration is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        display_element.querySelector('#jspsych-html-keyboard-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if trial_duration is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        end_trial();
      }, trial.trial_duration);
    }

  };

  return plugin;
})();
