var initVoice = function() {
if (annyang) {
  inputFunc = function(command, func) {
      // alert(command);
      Shiny.onInputChange('command', func.concat(' ', command));
  };
  arrange = function(command) {
      inputFunc(command, 'arrange');
  };
  filter = function(command) {
      inputFunc(command, 'filter');
  };
  reset = function() {
      inputFunc('', 'reset');
  };

  var commands3 = {
    'sort *command': arrange,
    'arrange *command': arrange,
    'order *command': arrange,
    'subset row *command': filter,
    'subset rows *command': filter,
    'grab rows *command': filter,
    'filter *command': filter,
    'reset': reset
  } ;
  var commands = {
    'title *title': function(title) {
      Shiny.onInputChange('title', title);
    },
    'color :color': function(color) {
      Shiny.onInputChange('color', color);
    },
    'bigger': function() {
      bigger += 1;
      Shiny.onInputChange('bigger', bigger);
    },
    'smaller': function() {
      if (bigger >= 1.5) {
        bigger -= 1;
        Shiny.onInputChange('bigger', bigger);
      }
    },
    'regression': function() {
      Shiny.onInputChange('yes', Math.random());
    }
  };
  // annyang.addCommands(commands);
  // annyang.addCommands(hello_commands);
  annyang.addCommands(commands3);
  annyang.start();
  }
};

$(function() {
  setTimeout(initVoice, 10);
});

