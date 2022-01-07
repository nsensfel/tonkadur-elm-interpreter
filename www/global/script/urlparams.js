/******************************************************************************/
/** URL Parameters ************************************************************/
/******************************************************************************/

/***
 * This module uses parses the current page's URL and converts it to a format
 * that can be used by Elm.
 **/

var tonkadur = tonkadur || new Object();

tonkadur.urlparams = new Object();

tonkadur.urlparams.private = new Object();

tonkadur.urlparams.private.get_clean_parameters_string =
function ()
{
   var all_parameters_string = window.location.search;

   if (all_parameters_string == "")
   {
      return "";
   }

   if (all_parameters_string[0] == "?")
   {
      return all_parameters_string.slice(1);
   }

   return all_parameters_string;
}

tonkadur.urlparams.get_parameters =
function ()
{
   var all_parameters_string =
      tonkadur.urlparams.private.get_clean_parameters_string();

   var all_parameters_array = all_parameters_string.split('&');

   var result = [];

   all_parameters_array.forEach
   (
      function (param)
      {
         result.push(param.split('='));
      }
   );

   return result;
}
