function [C, sigma] = dataset3Params(X, y, Xval, yval)
%dataset3Params returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = dataset3Params(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== Felipe's HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

log10_seq = -2:0.5:2;
C_tries = 10.^log10_seq;
sigma_tries = 10.^log10_seq;
n_tries = length(C_tries);


%first try
model_tried = svmTrain(X,y,C, @(x1, x2) gaussianKernel(x1, x2, sigma));
predictions = svmPredict(model_tried,Xval);
prediction_error_min = mean(double(predictions ~= yval));

for i=1:n_tries
    for j=1:n_tries
        model_tried = svmTrain(X,y,C_tries(i), @(x1, x2) gaussianKernel(x1, x2, sigma_tries(j)));
        predictions = svmPredict(model_tried,Xval);
        prediction_error = mean(double(predictions ~= yval));
        if (prediction_error < prediction_error_min)
            prediction_error_min =  prediction_error;
            C = C_tries(i);
            sigma = sigma_tries(j);
        end
    end
end






% =========================================================================

end
