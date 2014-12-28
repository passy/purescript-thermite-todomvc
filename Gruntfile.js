module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],
    
    clean: ["tmp", "output"],
  
    psc: {
      options: {
        main: "Main",
        modules: ["Main"]        
      },
      example: {
        dest: "js/Main.js",
        src: ["<%=libFiles%>"]
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
 
  grunt.registerTask("default", ["clean", "psc:example"]);
};
