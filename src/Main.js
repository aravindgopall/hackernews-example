var axios = require("axios");

exports["callApi"] = function(req, eCB, sCB){
  return function(){
    axios({
      method: req.metod,
      url : req.url,
      headers: req.headers,
      data: req.body
    })
    .then(function(resp) {
      //console.log("Response" , resp.data);
      sCB(resp.data)();
    }).catch(function(err) {
      console.log("Error occuer", err);
      eCB(err)();
    });
  }
}
