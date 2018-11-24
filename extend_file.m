function x = extend_file(existing_file, date_start)

% function x = extend_file(existing_file, date_start )
%
% Extends a file containing an MATLAnswer object named `x,` to include newer answers. The file name
% is specified as the char vector `existing_file`. The second input, `date_start`, indicates a
% date, in format '2018-06-20', up to which that file contains answers. The file is assumed to contain
% all answers corresponding to dates before the specified date. On the other hand, newer answers may
% exist in the specified date, or after the date, that are not in the file; and those will get included.
%
% This is useful because often it is not possible to download all answers up to the current date
% (probably because they are too many and the Stack Exchange API imposes some limitation in the number
% of answers that can be downloaded).
%
% This script downloads new answers starting from the specified date (which must be the date in which
% the existing file was generated), and merges the new and the old. When merging, older duplicates
% are removed. Duplicates arise if the existing file contains some answers in the specified date or
% after that. Those answers will be downloaded again, and the old version should be removed.
%
% Note that answers in the site before the specified may have changed and will not be updated. Only
% answers newer than the specified date are downloaded. If the existing file contains answers up to
% date D2, and an earlier date D1 is specified as `date_start`, answers from D1 to D2 will get updated.
%
% So the idea is to specify `date_start` as old as possible, to update existing answers in
% addition to include the new answers.
%
% Example use: x = extend_file('12viii18.mat', '2018-01-15');
%
% Luis Mendo

load(existing_file, 'x');
assert(logical(exist('x', 'var')), 'File does not contain a variable named x')
x_old = x;
x_new = MATLAnswer.fetch('fromdate', date_start); % 1 is most recent, end is earliest
x = [x_new; x_old]; % from most recent to earliest
x = flip(x); % from earliest to recent
creationDate = cellfun(@datenum, {x.CreationDate}); % convert from datetime to number. Thanks to the
% previous flip, this will be an increasing vector
[~, ind_keep] = unique(creationDate, 'last'); % 'last', together with the fac that creationDate is
% increasing, ensures we keep the most recently obtained version of each duplicate
ind_remove = setdiff(1:numel(x), ind_keep);
disp(['Removing ' num2str(numel(ind_remove)) ' duplicate answers; dates from ' ...
    datestr(x(ind_remove(1)).CreationDate, 1)  ' to ' datestr(x(ind_remove(end)).CreationDate, 1)])
disp(['The result contains ' num2str(numel(ind_keep)) ' answers'])
x = x(ind_keep);
x = flip(x); % undo previous flip, so that most recent appears first, as returned by MATLAnswer
% save(datestr(now,1), 'x')