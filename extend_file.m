function x = extend_file(existing_file, date_available)

% function x = extend_file(existing_file, date_available)
%
% Extends a file containing an MATLAnswer object named `x,` to include newer answers. The file name
% is specified as the char vector `date_available`. The second input, `data_available`, indicates the
% date, in format '2018-06-20', up to which that file contains answers. The file is assumed to contain
% all answers corresponding to dates before the specified date. On the other hand, newer answers may
% exist in the specified date that are not in the file; and those will be included.
%
% This is useful because often it is not possible to download all answers up to the current date
% (probably because they are too many and the Stack Exchange API imposes some limitation in the number
% of answers that can be downloaded).
%
% This script downloads new answers starting from the specified date (which must be the date in which
% the existing file was generated), and merges the new and the old. When merging, duplicates are removed.
% Duplicates may arise if the existing file contains some answers in the specified date. Those answers
% will be downloaded again, and should be removed. Two criteria are used for checking duplicates:
% URL and CreationDate. If the two criteria give different results an error is issued.
%
% Note that old answers in the site may have changed and will not be updated. Only answers newer than
% the specified date are downloaded.
%
% Example use: x = extend_file('12viii18.mat', '2018-08-12');
%
% Luis Mendo

load(existing_file, 'x');
assert(logical(exist('x', 'var')), 'File does not contain a variable named x')
x_old = x;
x_new = MATLAnswer.fetch('fromdate', date_available); % 1 is most recent, end is earliest
x = [x_new; x_old];
[~, ind_keep] = unique({x.URL}, 'stable');
creationDate = cellfun(@char, {x.CreationDate}, 'UniformOutput', false); % convert from datetime to char
[~, ind_keep_2] = unique(creationDate, 'stable'); % we need 'stable' so that ind_keep and ind_keep_2 have
% the same order
assert(isequal(ind_keep, ind_keep_2), 'Criteria for duplicates give different results')
ind_remove = setdiff(1:numel(x), ind_keep);
for k = ind_remove
    disp(['Removing duplicate answer, date ' char(x(k).CreationDate)])
end
x = x(ind_keep);
% save(datestr(now,1), 'x')