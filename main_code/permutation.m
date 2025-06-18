% ------------------------------------------------------------
% Permutation Test for Evaluating AUC Significance of SVM Classifier
% ------------------------------------------------------------
% This script evaluates whether the observed mean AUC from SVM classification
% is significantly better than chance using a non-parametric permutation test.
%
% Version: 1.0
% ------------------------------------------------------------

% ----------------------
% Parameters
% ----------------------
num_permutations = 10000;     % Number of permutations
num_folds = 2;                % Stratified 2-fold CV
cmd = sprintf('-s 0 -c %f -w1 %f -w2 %f', best_C, w1, w2);  % Use tuned parameters

[m, ~] = size(feature);       % Get data size

% ----------------------
% Prepare ground truth
% ----------------------
fprintf('Computing original AUC from trained model...\n');
original_AUC = mean(mean_AUC);  % Use previously computed average AUC
permuted_AUCs = zeros(num_permutations, 1);  % Store AUCs for permutations

% ----------------------
% Permutation Loop
% ----------------------
fprintf('Starting permutation test (%d permutations)...\n', num_permutations);

for perm = 1:num_permutations
    fprintf('Permutation %d / %d\n', perm, num_permutations);

    % Shuffle the class labels
    shuffled_labels = gindex3(randperm(length(gindex3)));

    % Get new group indices
    group1_idx = find(shuffled_labels == 1);
    group2_idx = find(shuffled_labels == 2);

    % Stratified folds for each group
    folds1 = crossvalind('Kfold', numel(group1_idx), num_folds);
    folds2 = crossvalind('Kfold', numel(group2_idx), num_folds);

    fold_auc = zeros(num_folds, 1);  % Store fold AUCs

    % Cross-validation loop
    for fold = 1:num_folds
        % Define test/train indices
        test_idx = [group1_idx(folds1 == fold); group2_idx(folds2 == fold)];
        train_idx = [group1_idx(folds1 ~= fold); group2_idx(folds2 ~= fold)];

        % Extract data
        X_train = double(feature(train_idx, :));
        X_test  = feature(test_idx, :);
        y_train = shuffled_labels(train_idx);
        y_test  = shuffled_labels(test_idx);

        % Train SVM on permuted labels
        model = svmtrain(y_train, sparse(X_train), cmd);

        % Predict
        [~, ~, dec_values] = svmpredict(y_test, sparse(X_test), model);

        % Compute AUC for the current fold
        binary_labels = (y_test == 1);  % Convert to binary
        [~, ~, ~, auc_val] = perfcurve(binary_labels, dec_values, 1);
        fold_auc(fold) = auc_val;
    end

    % Store the average AUC for this permutation
    permuted_AUCs(perm) = mean(fold_auc);
end

% ----------------------
% Compute Empirical p-value
% ----------------------
p_value = mean(permuted_AUCs >= original_AUC);

% ----------------------
% Output Results
% ----------------------
fprintf('\n-----------------------------------------\n');
fprintf('Permutation-based p-value: %.5f\n', p_value);
fprintf('Original AUC: %.4f\n', original_AUC);
fprintf('Mean Permuted AUC: %.4f\n', mean(permuted_AUCs));
fprintf('-----------------------------------------\n');

save('permutation_test_results.mat', 'permuted_AUCs', 'original_AUC', 'p_value');
