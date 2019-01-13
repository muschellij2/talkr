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
  not_filter = function(command) {
      inputFunc(command, 'filter out');
  };
  group_by = function(command) {
      inputFunc(command, 'group_by');
  };
  reset = function() {
      inputFunc('', 'reset');
  };


  var commands3 = {
    'sort *command': arrange,
    'arrange *command': arrange,
    'order *command': arrange,
    'group by *command': group_by,
    'group *command': group_by,

    'subset row *command': filter,
    'subset rows *command': filter,
    'grab rows *command': filter,
    'keep rows *command': filter,
    'drop rows *command': not_filter,
    'remove rows *command': not_filter,

    'subset rose *command': filter,
    'subset rose *command': filter,
    'grab rose *command': filter,
    'keep rose *command': filter,
    'drop rose *command': not_filter,
    'remove rose *command': not_filter,

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

