# Manual

## Create Surveys

1. At the (home page)[http://ec2-184-73-150-230.compute-1.amazonaws.com/static/index.html], click `Create` button on the left side of the page.

2. Upload your [survey config file](Specification.md). You can try a sample config [here](sample.json)

3. If your configuration file is parsed correctly, you will be taken to the Preview page where you can preview the survey just built. Otherwise, an error message will appear at the top.

4. If everything in the Preview page looks good. You can go on to upload this survey by clicking the `Submit` tab in the navigation menu.

5. Choose a name for your survey, this will be used by users to search and fill your survey. (There are two other fields for password and admin, which are just placeholders for now without any functionality). If the survey name is being taken, an error message will appear, otherwise, you will get a message telling you the survey is successfully created.

## Fill Surveys
1. At the right side of the (home page)[http://ec2-184-73-150-230.compute-1.amazonaws.com/static/index.html], fill the name of the survey you are going to fill and fill the name section, which will be used to identify your responses.

2. If the name exists, you will be taken to the `Survey` page where you can start filling the survey, otherwise there is an error message.

3. Start filling the survey, the responses will be saved instantly, for both radio group questions and text input. There will be flags indicating the responses are synced to the server.

4. You can always resume the survey after leaving the website by searching the survey name with the same user name at step 1. The survey will restore the the state you left.

## Get Results
Currently, the results could be accessed by anyone with the survey name. Which will be changed in the future. After searching the survey, click the `Responses` tab in the menu. There will be a download button to download all the responses related to this survey.