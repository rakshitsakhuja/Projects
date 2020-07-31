import functools
import glob
import operator
import os
from pprint import pprint

import numpy as np
import torch
from sklearn import preprocessing, model_selection, metrics
from torch.utils import data

import config
import engine
from checkpoint import save_ckp
from dataset import CaptchaImageDataset
from model import CaptchaModel


import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')
import numpy as np


def remove_duplicates(x):
    if len(x) < 2:
        return x
    fin = ""
    for j in x:
        if fin == "":
            fin = j
        else:
            if j == fin[-1]:
                continue
            else:
                fin = fin + j
    return fin


def decode_predictions(prediction, encoder):
    prediction = prediction.permute(1, 0, 2)
    prediction = torch.softmax(prediction, 2)
    prediction = torch.argmax(prediction, 2)
    prediction = prediction.detach().cpu().numpy()
    cap_prediction = []
    for j in range(prediction.shape[0]):
        temp = []
        for k in prediction[j, :]:
            k = k - 1
            if k == -1:
                temp.append("~")
            else:
                temp.append(encoder.inverse_transform([k])[0])

        tp = "".join(temp).replace("~", "")
        cap_prediction.append(remove_duplicates(tp))
    return cap_prediction


def run_training():
    image_files = glob.glob(os.path.join(config.DATA_DIR, "*.png"))
    targets_orig = [os.path.splitext(os.path.basename(i))[0] for i in image_files]

    targets = [[c for c in i] for i in targets_orig]
    targets_flat = functools.reduce(operator.iconcat, targets, [])
    # [j for i in targets for j in i ]
    lbl_enc = preprocessing.LabelEncoder()
    lbl_enc.fit(targets_flat)
    target_enc = [lbl_enc.transform(i) for i in targets]
    target_enc = np.array(target_enc) + 1

    train_images, test_images, train_targets, test_targets, train_orig_targets, test_orig_targets = model_selection.train_test_split(
        image_files, target_enc, targets_orig, test_size=0.1, random_state=42)
    # print(train_images[0], test_images[0], train_targets[0], test_targets[0], train_orig_targets[0], test_orig_targets[0])

    train_data = CaptchaImageDataset(
        image_paths=train_images,
        targets=train_targets,
        resize=(config.IMAGE_HEIGHT, config.IMAGE_WIDTH)
    )

    train_loader = data.DataLoader(
        train_data,
        batch_size=config.BATCH_SIZE,
        num_workers=config.NUM_WORKERS,
        shuffle=True
    )

    test_data = CaptchaImageDataset(
        image_paths=test_images,
        targets=test_targets,
        resize=(config.IMAGE_HEIGHT, config.IMAGE_WIDTH)
    )
    test_loader = data.DataLoader(
        test_data,
        batch_size=config.BATCH_SIZE,
        num_workers=config.NUM_WORKERS,
        shuffle=False
    )

    model = CaptchaModel(num_chars=len(lbl_enc.classes_))
    model.to(config.DEVICE)

    optimizer = torch.optim.Adam(model.parameters(),
                                 lr=3e-4)
    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        optimizer,
        factor=0.8,
        patience=5,
        verbose=True
    )
    print('done')

    losses = []
    accuracy_scores = []
    best_loss = 1e10
    best_accuracy=0
    for epoch in range(config.EPOCHS):
        train_loss = engine.train_fn(model, train_loader, optimizer)
        test_prediction, test_loss = engine.eval_fn(model, test_loader)


        # print(test_prediction,test_loss)

        #     print(type(test_prediction[0]))
        test_cap_prediction = []
        for kk in test_prediction:
            current_prediction = decode_predictions(kk, lbl_enc)
            # print(current_prediction[0])
            test_cap_prediction.extend(current_prediction)
        # print(test_cap_prediction)
        combined = list(zip(test_orig_targets, test_cap_prediction))
        # print(combined[:10])
        test_dup_rem = [remove_duplicates(c) for c in test_orig_targets]
        # print(test_dup_rem)
        accuracy = metrics.accuracy_score(test_dup_rem, test_cap_prediction)
        accuracy_scores.append(accuracy)
        losses.append(test_loss)
        if best_loss > test_loss:
            best_loss = test_loss
            best_accuracy = accuracy
            checkpoint = {
                'epoch': epoch + 1,
                'best_loss': test_loss,
                'state_dict': model.state_dict(),
                'optimizer': optimizer.state_dict()}
            save_ckp(checkpoint, True, config.checkpoint_path, config.best_model_path)
            print('Saving Best Model')
        print(f"Epoch: {epoch},   train_loss:{train_loss},  test_loss:{test_loss},   Accuracy={accuracy}")
        scheduler.step(test_loss)

    print(f"Accuracy of Model is {best_accuracy}  and Test Loss is {best_loss,}")
    fig = plt.figure()
    ax = plt.axes()


    ax.plot(accuracy_scores,label='Accuracy Scores');
    plt.show()

    fig = plt.figure()
    ax = plt.axes()
    ax.plot(losses,label='Loss Values');
    plt.show()







if __name__ == "__main__":
    run_training()
