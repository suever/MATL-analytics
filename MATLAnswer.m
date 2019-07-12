classdef MATLAnswer < handle
    % MATLAnswer - class to represent a MATL answer
    %
    %   This class works in conjunction with the Stack Exchange API
    %   to download MATL answers that were used to solve challenges
    %   on https://codegolf.stackexchange.com. Using this class, you
    %   can gather usage statistics of various MATL functions including
    %   input/output argument specifications.
    %
    % EXAMPLES:
    %
    %   Fecthing All Answers
    %       answers = MATLAnswer.fetch();
    %
    %   Fetching Answers After a Date
    %       recentAnswers = MATLAnswer.fetch('fromdate', '2016-03-01');
    %
    %   Create Plots of Usage
    %       MATLAnswer.plots();
    %
    %   Using the MATLAnswer Object
    %       answers = MATLAnswer.fetch();
    %
    %       % Get a list of functions that are used
    %       funcs = functions(answers);
    %
    %       % Get a list of input arguments used
    %       [inputargs, funcs] = specifiedInputs(answers);
    %
    %       % Get a list of output arguments used
    %       [outputargs, funcs] = specifiedOutputs(answers);
    %
    %       % Create a histogram of function usage
    %       h = functionHistogram(answers);
    %
    %       % Create a histogram of meta function usage
    %       h = metaHistogram(answers);
    %
    %       % Check which answers use a given function
    %       hasMod = uses(answers, '\');
    %       answers_with_mod = answers(hasMod);
    %
    %       % Check which answers use a function with a specific input spec
    %       hasSpec = uses(answers, '2$l');
    %       two_input_ones_answers = answers(hasSpec);
    %
    %       % Check which answers use a function with a specific out spec
    %       hasSpec = uses(answers, '2#f');
    %       two_output_find_answers = answers(hasSpec);
    %
    %       % Open a browser to one of the answers
    %       open(answers(1))
    %
    %       % Open a tab for each of the first four answers
    %       open(answers(1:4))

    properties
        MetaData        % additional metadata returned by the API
        Source          % MATL source code for the answer
        Valid = true    % Flag to indicate whether this is valid MATL
    end

    properties (Dependent)
        Accepted        % Whether an answer was accepted or not
        Hyperlink       % Link to the original answer
        ID              % ID of the answer
        Score           % Score of the answer
        QuestionID      % ID of the question
        CreationDate    % Date an answer was initially created
        LastActivity    % Date an answer was last edited
        Owner           % ID of the owner of the question
        URL             % URL of the answer
    end

    properties (Hidden, Constant)
        % The URL to the stackexchange API
        API_URL = 'https://api.stackexchange.com/2.2';
    end

    properties (Hidden)
        Parts           % Results of matl_parse on the source code
    end

    methods
        function self = MATLAnswer(metadata)
            % MATLAnswer - constructor for the MATLAnswer class
            %
            % USAGE:
            %   A = MATLAnswer(metadata)
            %
            % INPUTS:
            %   metadata:   Struct, information returned from the stack
            %               exchange API.
            %
            %
            % OUTPUTS:
            %   A:          Handle, Handle to the MATLAnswer object.


            % If an array of metadata structs are passed in, create a
            % MATLAnswer object for each of them
            if numel(metadata) > 1
                objs = arrayfun(@MATLAnswer, metadata, 'uni', 0);
                self = reshape(cat(1, objs{:}), size(metadata));
                return;
            end

            % Store the metadata
            self.MetaData = metadata;

            % Extract the source code component. For this we use
            % <pre><code> in the HTML rather than the Markdown just because
            % code can be posted as either indented or surrounded by ``. We
            % use the HTML so SO can deal with that conversion for us.

            % We also grab the first code block (with no newlines). This
            % prevents us from grabbing explanation text if it is in a
            % separate or the same codeblock.
            pattern = '(?<=<pre><code>[\s\n]*)[^\n]*';
            code = regexp(metadata.body, pattern, 'match', 'once');

            % Now convert any HTML encoded symbols (<, >, &, etc.)
            import org.apache.commons.lang.StringEscapeUtils;
            self.Source = char(StringEscapeUtils.unescapeHtml(code));

            % Now use the MATL parser to actually parse the source code
            try
                self.Parts = matl_parse(self.Source, false);
            catch ME
                % If there was a parse error, then just mark as invalid
                if strncmpi(ME.identifier, 'MATL:parser', 11)
                    self.Valid = false;
                else
                    rethrow(ME);
                end
            end
        end

        function [f, ind] = functions(self)
            % functions - List of functions that are used by the answers
            %
            %   This listing excludes all literal numbers and literal
            %   strings.
            %
            % USAGE:
            %   [F, ind] = functions(self)
            %
            % INPUTS:
            %   self:   Handle, Scalar or array of MATLAnswer objects
            %
            % OUTPUTS:
            %   F:      [1 x N] Cell Array, Strings representing all
            %           functions that were used by any of the MATLAnswer
            %           objects passed to the function.
            %
            %   ind:    [1 x N] Array, Indices into the input object
            %           indicating which object used a particular function.

            % If an array was passed in, parse each entry and concatenate
            % the results.
            if numel(self) > 1
                f = arrayfun(@functions, self, 'uni', 0);

                % Figure out how many outputs were from each object
                ind = repelem(1:numel(f), cellfun(@numel, f));
                f = cat(2, f{:});
                return
            end

            % This is going to exclude number literals and string literals
            toexclude = {'literal.number', 'literal.string'};
            isliteral = ismember({self.Parts.type}, toexclude);

            f = {self.Parts(~isliteral).source};
            ind = ones(size(f));
        end

        function [inputs, funcs, inds] = specifiedInputs(self)
            % specifiedInputs - Get all instances where $ was used
            %
            %   This function returns all instances where $ was used to
            %   indicate the number of input arguments. The result is a
            %   cell array indicating what the preceding value for $ was as
            %   well as the function for which $ was being used.
            %
            % [inputs, functions, inds] = specifiedInputs(self)
            %
            % INPUTS:
            %   self:       Handle, Scalar or array of MATLAnswer objects
            %
            % OUTPUTS:
            %   inputs:     [1 x N] Cell Array, Cell array containing what
            %               the specified number of inputs actually was.
            %               All values (even if numeric) are returned as
            %               strings for consistency.
            %
            %   functions:  [1 x N] Cell Array, Strings corresponding to
            %               the previous output variable which indicate
            %               which MATL function the input specification was
            %               applied to.
            %
            %   inds:       [1 x N] Array, Index values corresponding to
            %               which of the input objects each of the outputs
            %               belongs to.

            % If an array of answers were provided, compute this for all of
            % them and concatenate the results
            if numel(self) > 1
                [I, F] = arrayfun(@specifiedInputs, self, 'uni', 0);

                inds = repelem(1:numel(I), cellfun(@numel, I));
                inputs = cat(2, I{:});
                funcs = cat(2, F{:});
                return
            end

            % Find all usages of $ (metaFunction.inSpec)
            isInput = ismember({self.Parts.type}, 'metaFunction.inSpec');

            % Find the numbers that preceded $
            inputs = self.findNumericInputs(self.Parts, isInput, self);

            % Find the first function call after each $ usage
            funcs = self.findNextFunction(self.Parts, isInput);

            % Remove empty results (ones which failed validation)
            toremove = cellfun('isempty', inputs);
            inputs(toremove) = [];
            funcs(toremove) = [];

            % Filler for the indices
            inds = ones(size(inputs));
        end

        function [outputs, funcs, inds] = specifiedOutputs(self)
            % specifiedOutputs - Get all instances where # was used
            %
            %   This function returns all instances where $ was used to
            %   indicate the number of input arguments. The result is a
            %   cell array indicating what the preceding value for $ was as
            %   well as the function for which $ was being used.
            %
            % [outputs, functions, inds] = specifiedOutputs(self)
            %
            % INPUTS:
            %   self:       Handle, Scalar or array of MATLAnswer objects
            %
            % OUTPUTS:
            %   ouptuts:    [1 x N] Cell Array, Cell array containing what
            %               the specified number of outputs actually was.
            %               All values (even if numeric) are returned as
            %               strings for consistency.
            %
            %   functions:  [1 x N] Cell Array, Strings corresponding to
            %               the previous output variable which indicate
            %               which MATL function the output specification
            %               was applied to.
            %
            %   inds:       [1 x N] Array, Index values corresponding to
            %               which of the input objects each of the outputs
            %               belongs to.

            % If an array of answers were provided, compute this for all of
            % them and concatenate the results
            if numel(self) > 1
                [outs, F] = arrayfun(@specifiedOutputs, self, 'uni', 0);

                inds = repelem(1:numel(outs), cellfun(@numel, outs));
                outputs = cat(2, outs{:});
                funcs = cat(2, F{:});
                return
            end

            % Find all usages of # (metaFunction.outSpec)
            isOutput = ismember({self.Parts.type}, 'metaFunction.outSpec');

            % Find the number that preceded #
            outputs = self.findNumericInputs(self.Parts, isOutput, self);

            % Find the first function call after each # usage
            funcs = self.findNextFunction(self.Parts, isOutput);

            % Remove empty results (ones which failed validation)
            toremove = cellfun('isempty', outputs);
            outputs(toremove) = [];
            funcs(toremove) = [];

            % Filler for the indices
            inds = ones(size(outputs));
        end

        function stat = open(self, browser, new)
            % open - Opens a given answer in the MATLAB browser
            %
            % USAGE:
            %   stat = open(self, browser, new)
            %
            % INPUTS:
            %   browser:    Logical, Indicates whether to use the system
            %               browser (true) or not (false) [Default = false]
            %
            %   new:        Logical, Indicates whether to open the answer
            %               in a new tab (true) or not (false). If an array
            %               of answers are provided, this open is ignored
            %               and a tab is opened per answer. [Default =
            %               true]
            %
            % OUTPUTS:
            %   stat:       Integer, Indicates the status of the 'web'
            %               command in MATLAB. See built-in help for 'web'
            %               for more information

            flags = {};

            system_browser = exist('browser', 'var') && browser;

            if system_browser
                flags{end+1} = '-browser';
            elseif (~exist('new', 'var') || new) || numel(self) > 1
                flags{end+1} = '-new';
            end

            % Find the answers with unique URLs
            stat = cellfun(@(x)web(x, flags{:}), {self.URL});
        end

        function bool = uses(self, func)
            % uses - Determines whether the answer uses a specific function
            %
            %   This method determines whether a given MATL answer uses a
            %   particular function. In addition to a standard function
            %   name, you can specify the input/output spec to filter the
            %   result to only those answers that use that function AND use
            %   the specified input/output spec.
            %
            % USAGE:
            %   bool = uses(self, func)
            %
            % INPUTS:
            %   func:   String, Function to check usage for. This can
            %           either be a plain function call (i.e. 'f') or a
            %           function with a specific input spec (i.e. '2$f') or
            %           output spec (i.e. '2#f')
            %
            % OUTPUTS:
            %   bool:   Logical, A logical array where the value is true if
            %           the function was used in a particular answer or
            %           false if it was not. This is an array the same size
            %           as the object array passed to this function.

            if numel(self) > 1
                bool = arrayfun(@(x)uses(x, func), self);
                return;
            end

            % First check to see if input or output spec is here.
            parts = matl_parse(func, false);

            % Remove the implicit ones
            parts = parts(~[parts.implicit]);

            % First check for the function
            isFunction = ismember({parts.type}, 'function');

            if sum(isFunction) > 1
                error(sprintf('%s:InvalidInput', mfilename), ...
                    'You can only specify one function');
            elseif ~any(isFunction)
                error(sprintf('%s:InvalidInput', mfilename), ...
                    'You must specify one function');
            end

            func = parts(isFunction).source;

            % Now look at functions
            funcs = functions(self);
            bool = ismember(func, funcs);

            if ~bool; return; end

            % Check for input spec
            [tf, ind] = ismember('metaFunction.inSpec', {parts.type});

            if tf
                % Then filter the results
                nInputs = parts(ind-1).source;
                [inSpec, funcs] = specifiedInputs(self);

                % Look for where the function and spec match
                ismatch = ismember(inSpec, nInputs) & ...
                          ismember(funcs, func);

                bool = bool && any(ismatch);

                if ~bool; return; end
            end

            [tf, ind] = ismember('metaFunction.outSpec', {parts.type});

            if tf
                nOutputs = parts(ind-1).source;
                [outSpec, funcs] = specifiedOutputs(self);

                ismatch = ismember(outSpec, nOutputs) & ...
                          ismember(funcs, func);

                bool = bool && any(ismatch);

                if ~bool; return; end
            end
        end

        function h = functionHistogram(self)
            % functionHistogram - Create an image showing function usage
            %
            % USAGE:
            %   h = functionHistogram(self)
            %
            % INPUTS:
            %   self:       Handle, Scalar or array of MATLAnswer objects
            %
            % OUTPUTS:
            %   h:          Graphics Handle, handle to the pcolor plot
            %               object showing the histogram.

            % Get a list of all functions that are used
            F = functions(self);

            % All possible functions and modifiers
            rows = ['!"#$%&''()*+,-./0123456789:;<=>?@', ...
                    'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ...
                    '[\]^_`abcdefghijklmnopqrstuvwxyz{|}~'];
            rows = num2cell(rows);
            modifiers = {'', 'X', 'Y', 'Z'};

            % Compute the number of occurances for each one
            results = zeros(numel(rows) + 1, numel(modifiers) + 1);
            for k = 1:numel(modifiers)
                combos = strcat(modifiers{k}, rows);

                [~, inds] = ismember(F, combos);

                for m = 1:numel(combos)
                    results(m,k) = sum(inds == m);
                end
            end

            % Show the histogram image
            h = self.histogramImage(results, modifiers, rows);

            % Update the title
            title('MATL Function Usage')
        end

        function h = metaHistogram(self, varargin)
            % metaHistogram - Create an image showing metafunction usage
            %
            % USAGE:
            %   h = metaHistogram(self)
            %
            % INPUTS:
            %   self:       Handle, Scalar or array of MATLAnswer objects
            %
            % OUTPUTS:
            %   h:          Graphics Handle, handle to the pcolor plot
            %               object showing the histogram.

            % Get all input specifications
            [inputs, inputfuncs] = specifiedInputs(self);

            % Get all output specifications
            [outputs, outputfuncs] = specifiedOutputs(self);

            % Add $ to all input specs and # to all output specs and sort
            [inputs, iind] = sort(strcat(inputs, '$'));
            [outputs, oind] = sort(strcat(outputs, '#'));

            % Apply the sorting to the function names as well
            inputfuncs = inputfuncs(iind);
            outputfuncs = outputfuncs(oind);

            % Combine the input/output data
            nargs = cat(1, inputs(:), outputs(:));
            funcs = cat(1, inputfuncs(:), outputfuncs(:));

            % Now compute the histogram
            [uniqueArgs, ~, ib] = unique(nargs, 'stable');
            [uniqueFunctions, ~, fb] = unique(funcs);

            results = zeros(numel(uniqueFunctions)+1, numel(uniqueArgs)+1);

            for k = 1:numel(uniqueFunctions)
                for m = 1:numel(uniqueArgs)
                    results(k,m) = sum(ib == m & fb == k);
                end
            end

            % Display the histogram
            h = self.histogramImage(results, uniqueArgs, uniqueFunctions);

            % Now draw a thick vertical line between inputs/outputs
            xpos = numel(unique(inputs(:))) + 1;
            ylims = get(gca, 'ylim');
            axis('manual')
            plot([xpos, xpos], ylims, 'Color', 'k', 'linewidth', 3)

            % Update title/labels
            title('MATL Meta-Function Usage')
            xlabel('Explicit Number Inputs/Outputs')
            ylabel('Function')
        end
    end

    %--- Getters and Setters ---%
    methods
        function res = get.Hyperlink(self)
            % Convert the hyperlink to a clickable link
            url = self.URL;
            res = sprintf('<a href="%s">%s</a>', url, url);
        end

        function res = get.URL(self)
            format = 'http://codegolf.stackexchange.com/a/%d';
            res = sprintf(format, self.MetaData.answer_id);
        end

        function res = get.ID(self)
            res = self.MetaData.answer_id;
        end

        function res = get.Score(self)
            res = self.MetaData.score;
        end

        function res = get.QuestionID(self)
            res = self.MetaData.question_id;
        end

        function res = get.CreationDate(self)
            dt = self.MetaData.creation_date;
            res = datetime(dt, 'ConvertFrom', 'epochtime');
        end

        function res = get.LastActivity(self)
            dt = self.MetaData.last_activity_date;
            res = datetime(dt, 'ConvertFrom', 'epochtime');
        end

        function res = get.Accepted(self)
            res = self.MetaData.is_accepted;
        end

        function res = get.Owner(self)
            res = self.MetaData.owner.user_id;
        end
    end

    methods (Static, Access = 'protected')
        function h = histogramImage(data, xlabels, ylabels)
            % histogramImage - Creates a histogram-based image w/ labels
            %
            %   This helper function creates a pcolor instance showing the
            %   2D histogram that is passed to it. The color scales from 1
            %   (white) to the max (dark green). Any zeros are shown as
            %   gray.
            %
            % USAGE:
            %   h = MATLAnswer.histogramImage(data, xlabels, ylabels)
            %
            % INPUTS:
            %   data:       [M x N] Matrix, Contains the number of counts for
            %               each 2D bin
            %
            %   xlabels:    [1 x N] Cell Array, Labels for the x dimension
            %               of the histogram
            %
            %   ylabels:    [1 x M] Cell Array, Labels for the y dimension
            %               of the histogram
            %
            % OUTPUTS:
            %   h:          Graphics Handle, handle to the pcolor plot that
            %               was generated.

            % Determine the default width/height of the figure window
            height = max(300, size(data, 1) * 20);
            width = max(300, size(data, 2) * 30);

            figure('Position', [0 0 width height]);

            % Create the pcolor plot
            h = pcolor(data);

            % Compute the colormap to use
            N = max(data(:));
            CMAP = [linspace(1, 0, N); ...
                    linspace(1, 0.5, N); ...
                    linspace(1, 0, N)].';

            % Use gray for zero usages
            CMAP = cat(1, [0.8 0.8 0.8], CMAP);

            % Apply the colormap
            colormap(CMAP);

            % Place x/y labels as requested
            xpos = (1:size(data, 2)) + 0.5;
            ypos = (1:size(data, 1)) + 0.5;

            [xx,yy] = meshgrid(xpos, ypos);

            set(gca, 'xtick', xpos, 'xticklabels', xlabels, ...
                     'ytick', ypos, 'yticklabels', ylabels, ...
                     'LooseInset', [0 0 0 0], ...
                     'OuterPosition', [0 0 1 1]);

            hold on

            % Default text properties
            args = {'VerticalAlignment', 'middle', ...
                    'HorizontalAlignment', 'center', ...
                    'FontWeight', 'bold'};

            % Place all numeric labels (when non-zero)
            for k = 1:numel(data)
                if data(k)
                    text(xx(k), yy(k), num2str(data(k)), args{:});
                end
            end

            % Polish up the axes and figure
            set(gca, 'ydir', 'reverse', 'fontweight', 'bold')
            set(gcf, 'PaperPositionMode', 'auto')
        end

        function funcs = findNextFunction(data, functionIndex)
            % findNextFunction - Finds the next available function call
            %
            %   Given an index, this function seeks to find the next
            %   function call and returns the name of that function.
            %
            % USAGE:
            %   funcs = MATLAnswer.findNextFunction(data, index)
            %
            % INPUTS:
            %   data:   [1 x N] Struct, An array of structs where each
            %           entry is an entry from matl_parse. This is used to
            %           find the next function.
            %
            %   index:  [1 x N] Logical, or [1 x M] Integer, Reference
            %           index which indicates which entry in data we should
            %           use as the reference point for finding the next
            %           function.
            %
            % OUTPUTS:
            %   funcs:  [1 x M] Cell Array, An array of function names that
            %           were found to follow the specified indices.

            % Convert logical values to index-based values
            if islogical(functionIndex)
                functionIndex = find(functionIndex);
            end

            % Figure out which of the parse values are actually functions
            isFunction = find(ismember({data.type}, 'function'));

            funcs = cell(1, numel(functionIndex));

            % Find the next closest function after the current one
            for k = 1:numel(functionIndex)
                nextind = min(isFunction(isFunction > functionIndex(k)));
                funcs{k} = data(nextind).source;
            end
        end

        function inputs = findNumericInputs(data, functionIndex, obj)
            % findNumericInputs - Find the number that precedes a function
            %
            %   This function is used to identify the number which precedes
            %   a function, specifically $ or #. If it encounters a
            %   function such as q, Q, or t instead of an actual number, an
            %   attempt is made to look back in the stack for the number
            %   literal.
            %
            % USAGE:
            %   inputs = findNumericInputs(data, index, obj)
            %
            % INPUTS:
            %   data:   [1 x N] Struct, An array of structs where each
            %           entry is an entry from matl_parse. This is used to
            %           find the preceding number literal.
            %
            %   index:  [1 x N Logical or [1 x M] Integer, Reference
            %           index which indicates which entry in data we should
            %           use as the reference for finding the preceding
            %           number literal.
            %
            % OUTPUTS:
            %   inputs: [1 x M] Cell Array, The number literal (as a
            %           string) that was found to preceded each entry
            %           specified by index.

            % Convert logical indices into actual index values
            if islogical(functionIndex)
                functionIndex = find(functionIndex);
            end

            % Now locate the previous entry before the function
            previous = data(functionIndex - 1);

            inputs = cell(1, numel(previous));

            for k = 1:numel(previous)
                switch previous(k).type
                    % For number literals and logicals, use as-is
                    case {'literal.number', 'literal.logicalRowArray'}
                        inputs{k} = previous(k).source;
                    case 'function'
                        % Check to see if this function is a pre-defined
                        % number (H, I, K, N, etc.)
                        num = MATLAnswer.predefinedNumber(previous(k).source);

                        % If it was a pre-defined literal go to the next
                        if ~isempty(num)
                            inputs{k} = num;
                            continue;
                        end

                        % Otherwise we have a stack manipulation that we
                        % will try to work with
                        if ismember(previous(k).source, {'q', 't', 'Q'})
                            % Now we can go get the previous value
                            prevPrevious = data(functionIndex(k) - 2);

                            % Convert to a number (if possible)
                            num = MATLAnswer.predefinedNumber(prevPrevious.source);

                            if ~isempty(num)
                                switch previous(k).source
                                    case 'q'    % Decrement by 1
                                        increment = -1;
                                    case 't'    % Duplicate
                                        increment = 0;
                                    case 'Q'    % Increment by 1
                                        increment = 1;
                                end

                                % Special case when input is `N`
                                if isequal(num, 'N')
                                    inputs{k} = [num, previous(k).source];
                                    continue;
                                end

                                % Attempt to apply the increment
                                num = str2double(num) + increment;

                                % If there was a NaN something went wrong
                                if ~isnan(num)
                                    inputs{k} = num2str(num);
                                    continue;
                                end
                            end
                        end
                        % If we haven't parsed it, warn and move on
                        throwWarning(previous(k).source, obj)
                    otherwise
                        % If this is something we didn't account for, warn
                        % and move onto the next
                        throwWarning(previous(k).source, obj)
                end
            end

            function throwWarning(src, obj)
                % throws a warning with helpful debug info
                warning('Unexpected input argument: %s', src);
                disp(obj)
            end
        end

        function num = predefinedNumber(src)
            % Converts the source to a number literal (if possible)
            %
            % USAGE:
            %   num = MATLAnswer.predefinedNumber(src)
            %
            % INPUTS:
            %   src:    String, Source that we want to attempt to convert
            %           to a number literal.
            %
            % OUTPUTS:
            %   num:    String, Number literal (if possible), otherwise an
            %           empty array is returned.

            % Map all functions that are pre-defined number litearls
            mapper = {'O', '0';
                      'l', '1';
                      'H', '2';
                      'I', '3';
                      'K', '4';
                      'N', 'N'};

            % First try to convert if already a number literal
            if ~isnan(str2double(src))
                num = src;
                return
            end

            % Now look to map pre-defined functions to number literals
            [tf, ind] = ismember(src, mapper(:,1));

            if tf
                num = mapper{ind, 2};
            else
                % Return an empty string if this fails
                num = [];
            end
        end

        function data = getData(url, varargin)
            % getData - Makes a call to the API URL to retrieve data
            %
            % USAGE:
            %   data = MATLAnswer.getData(url, varargin)
            %
            % INPUTS:
            %   url:        String, URL to query, expects a JSON response
            %   varargin:   Any additional parameters to pass to webread
            %               to perform the query.
            %
            % OUTPUTS:
            %   data:       [M x 1] Cell Array, Each element contains a
            %               single object returned by webread.

            % Default parameters to pass to the API
            params = {'order', 'desc', ...
                      'sort', 'creation', ...
                      'pagesize', 100, ...
                      'site', 'codegolf'};

            % Loop until we have all records
            resp.has_more = true;
            page = 1;

            data = {};

            while resp.has_more
                resp = webread(url, params{:}, 'page', page, varargin{:});

                % If an array of structs was returned, convert to a cell
                % array for consistency
                if ~iscell(resp.items)
                    resp.items = num2cell(resp.items);
                end

                % Concatenate all data
                data = cat(1, data, resp.items(:));
                page = page + 1;
            end
        end
    end

    methods (Static)
        function plots(varargin)
            % plots - Create (and save) histogram plots
            %
            % USAGE:
            %   MATLAnswer.plots(varargin)
            %
            % INPUTS:
            %   varargin:   ..., Same inputs as to MATLAnswer.fetch()

            answers = MATLAnswer.fetch(varargin{:});

            functionHistogram(answers);
            print(gcf, 'MATL.Functions.png', '-dpng', '-r300');

            metaHistogram(answers);
            print(gcf, 'MATL.Meta.Usage.png', '-dpng', '-r300');
        end

        function res = fetch(ignore, varargin)
            % fetch - Fetch all current answers and return MATLAnswer's
            %
            %   This static method requests all current MATL answers using
            %   the stack exchange API and parses the result into an array
            %   of MATLAnswer objects. These objects can then be used to
            %   understand and visualize the way that MATL functions are
            %   currently being used.
            %
            % USAGE:
            %   R = MATLAnswer.fetch(toignore, pvpairs)
            %   R = MATLAnswer.fetch(pvpairs)
            %
            % INPUTS:
            %   toignore:   [1 x N] Array, Array of question IDs to ignore.
            %               By default we ignore cops/robbers threads as
            %               those never parse correctly due to the usage of
            %               # as an unknown character place-holder. Any
            %               specific question ID can be placed here.
            %
            %   pvpairs:    Parameter/value pairs, Any additional inputs
            %               that should be passed to the API to filter the
            %               result.
            %
            % OUTPUTS:
            %   R:          [M x 1] Object, Array of MATLAnswer objects
            %               which can be used to gather current MATL usage
            %               statistics.

            if exist('ignore', 'var') && ischar(ignore)
                % Then no ignores were specified
                varargin = cat(2, ignore, varargin);
                clear ignore
            end

            if ~exist('ignore', 'var')
                % Ignore all of the answers that were posted in
                % cops/robbers questions due to stray #
                ignore = 77419;
            end

            % Look in search/excerpts for MATL occurances
            url = strcat(MATLAnswer.API_URL, '/search/excerpts');
            answers = MATLAnswer.getData(url, 'q', 'MATL', varargin{:});

            % Determine which ones are *actual* answers (start with MATL)
            isAnswer = cellfun(@(x)strcmp(x.item_type, 'answer'), answers);
            answers = answers(isAnswer);

            % Convert the cell array of structs into an array of structs
            % now that they are all answers
            answers = cat(1, answers{:});

            isMATL = ~cellfun(@isempty, regexp({answers.body}, '^\s*MATL'));
            answers = answers(isMATL);

            % Now we want the actual content from the answers
            % The URL should have a semi-colon separated list of all
            % answers IDs that we care about. We only want to pass 100 at a
            % time though

            allanswers = {};
            chunksize = 100;

            for k = 1:ceil(numel(answers) / chunksize)
                chunk = answers(((k - 1) * chunksize + 1) : ...
                                (min(k * chunksize, numel(answers))));

                ids = sprintf('%d;', chunk.answer_id);
                url = strcat(MATLAnswer.API_URL, '/answers/', ids(1:end-1));
                data = MATLAnswer.getData(url, 'filter', '!9YdnSM64y');

                allanswers = cat(1, allanswers, data);
            end

            % Remove entries with last_edit_date and community_owned_date
            % since it's not present in all results
            for k = 1:numel(allanswers)
                if isfield(allanswers{k}, 'last_edit_date')
                    allanswers{k} = rmfield(allanswers{k}, 'last_edit_date');
                end
                if isfield(allanswers{k}, 'community_owned_date')
                    allanswers{k} = rmfield(allanswers{k}, 'community_owned_date');
                end
            end

            answers = cat(1, allanswers{:});

            % Ignore answers that were in the ignored list
            toremove = ismember([answers.question_id], ignore);
            answers(toremove) = [];

            % Now create an object instance for each answer
            res = MATLAnswer(answers);

            % Remove entries which were considered to be invalid
            res = res([res.Valid]);
        end
    end
end
