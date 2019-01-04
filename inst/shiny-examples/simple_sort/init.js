var initVoice = function() {
if (annyang) {
  Shiny.setInputValue('yes', 'no');
  inputFunc = function(command) {
      // alert(command);    
      Shiny.onInputChange('command', command);
  };
  // var commands2 = { 'run error': console.log("error") };
  helloFunction = function() {
    Shiny.onInputChange('title', Math.random());
    alert("hello");
  };  
  var hello_commands = {'hello :name': helloFunction, 'howdy': helloFunction};

  Shiny.onInputChange('title', 'say title something');
  Shiny.onInputChange('color', 'black');
  Shiny.onInputChange('bigger', 1);
  Shiny.onInputChange('yes', 'no');
  var commands3 = { 'run *command': inputFunc } ;
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
  annyang.addCommands(hello_commands);
  annyang.addCommands(commands3);
  annyang.start();
  }
};

$(function() {
  setTimeout(initVoice, 10);
});

