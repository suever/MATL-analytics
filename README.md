# MATL Analytics

This repository contains MATLAB/Octave code for analyzing MATL answers that 
have been submitted to [code golf challenges][codegolf].

## Contents

### `MATLAnswer`

A class which works in conjunction with the [Stack Exchange API][api] to
download MATL answers that were used to solve challenges. Using this class,
it is possible to get usage statistics of various MATL functions including
input and output argument specifications.

**Basic Usage**

Fetching all answers using the API

    answers = MATLAnswer.fetch();

Fetching only answers submitted after a particular date

    recent = MATLAnswer.fetch('fromdate', '2016-03-01');

Create usage plots

    MATLAnswer.plots()

See the help section of the file to see more detailed usage information.



## License

This software is licensed under the MIT License.  
Copyright (c) 2017 [Jonathan Suever][suever]

[api]: https://api.stackexchange.com/
[codegolf]: https://codegolf.stackexchange.com
[suever]: https://suever.net
