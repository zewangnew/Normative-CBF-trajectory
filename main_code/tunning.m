% ------------------------------------------------------------
% SVM Hyperparameter Tuning via Stratified 2-Fold Cross-Validation
% ------------------------------------------------------------
% This script performs grid search over C values for SVM classification
% using repeated stratified 2-fold cross-validation, and evaluates
% accuracy, precision, recall, F1-score, and AUC.
%
% Version: 1.0
% ------------------------------------------------------------

% ----------------------
% Setup and Parameters
% ----------------------
num_repeats = 100;          % Number of repetitions
C_values = meshgrid(-10:0.05:10);  % Grid over log2(C)
num_C = size(C_values, 2);  % Number of C values

[m, n] = size(feature);     % Data dimensions
group1_idx = find(gindex3 == 1);  % Group 1 indices
group2_idx = find(gindex3 == 2);  % Group 2 indices

% Compute class weights for unbalanced data
w1 = m / numel(group1_idx);
w2 = m / numel(group2_idx);

% ----------------------
% Initialize Result Matrices
% ----------------------
ALL_acc_repeats   = zeros(num_repeats, num_C);
precision_repeats = zeros(num_repeats, num_C);
recall_repeats    = zeros(num_repeats, num_C);
F1_score_repeats  = zeros(num_repeats, num_C);
AUC_repeats       = zeros(num_repeats, num_C);

% ----------------------
% Repeated CV Loop
% ----------------------
for repeat = 1:num_repeats
    fprintf('Repetition %d / %d\n', repeat, num_repeats);

    % Stratified 2-fold cross-validation for both groups
    folds1 = crossvalind('Kfold', numel(group1_idx), 2);
    folds2 = crossvalind('Kfold', numel(group2_idx), 2);

    % Iterate through all C values
    for subi = 1:num_C
        log2c = C_values(1, subi);
        c_exp = 2^log2c;

        % Construct training command with weight and C
        cmd = sprintf('-s 0 -c %f -w1 %f -w2 %f', c_exp, w1, w2);
        fprintf('Testing C = 2^%.2f = %.5f\n', log2c, c_exp);

        % Containers for per-fold results
        fold_accuracy = zeros(2, 1);
        fold_precision = zeros(2, 1);
        fold_recall = zeros(2, 1);
        fold_F1 = zeros(2, 1);
        fold_auc = zeros(2, 1);

        % Perform 2-fold CV
        for fold = 1:2
            % Get training/testing indices for both groups
            test_idx = [group1_idx(folds1 == fold); group2_idx(folds2 == fold)];
            train_idx = [group1_idx(folds1 ~= fold); group2_idx(folds2 ~= fold)];

            % Extract data
            X_train = double(feature(train_idx, :));
            X_test = feature(test_idx, :);
            y_train = gindex3(train_idx);
            y_test = gindex3(test_idx);

            % Train SVM
            model = svmtrain(y_train, sparse(X_train), cmd);

            % Predict and evaluate
            [y_pred, acc, scores] = svmpredict(y_test, sparse(X_test), model);
            fold_accuracy(fold) = acc(1);

            % Compute precision, recall, F1
            [confMat, ~] = confusionmat(y_test, y_pred);
            if size(confMat, 1) == 2
                tp = confMat(1, 1); fp = confMat(2, 1); fn = confMat(1, 2);
                precision = tp / (tp + fp + eps);
                recall = tp / (tp + fn + eps);
            else
                precision = 0;
                recall = 0;
            end

            fold_precision(fold) = precision;
            fold_recall(fold) = recall;
            fold_F1(fold) = 2 * (precision * recall) / (precision + recall + eps);

            % Compute AUC
            y_binary = y_test == 1;
            [~, ~, ~, auc] = perfcurve(y_binary, scores, 1);
            fold_auc(fold) = auc;
        end

        % Store mean values across folds
        ALL_acc_repeats(repeat, subi)   = mean(fold_accuracy);
        precision_repeats(repeat, subi) = mean(fold_precision);
        recall_repeats(repeat, subi)    = mean(fold_recall);
        F1_score_repeats(repeat, subi)  = mean(fold_F1);
        AUC_repeats(repeat, subi)       = mean(fold_auc);
    end
end

% ----------------------
% Aggregate and Report Results
% ----------------------
mean_accuracy = mean(ALL_acc_repeats, 1);
mean_precision = mean(precision_repeats, 1);
mean_recall = mean(recall_repeats, 1);
mean_F1 = mean(F1_score_repeats, 1);
mean_AUC = mean(AUC_repeats, 1);

[best_acc, best_idx] = max(mean_accuracy);
best_C = 2^C_values(1, best_idx);

% Display best parameters and performance
fprintf('Best log2(C): %.3f\n', C_values(1, best_idx));
fprintf('Best C: %.5f\n', best_C);
fprintf('Mean Accuracy: %.4f\n', best_acc);
fprintf('Mean Precision: %.4f\n', mean_precision(best_idx));
fprintf('Mean Recall: %.4f\n', mean_recall(best_idx));
fprintf('Mean F1-score: %.4f\n', mean_F1(best_idx));
fprintf('Mean AUC: %.4f\n', mean_AUC(best_idx));
