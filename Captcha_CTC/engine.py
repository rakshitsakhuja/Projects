import torch
from tqdm import tqdm
import config


def train_fn(model, data_loader, optimizer):
    model.train()

    fin_loss = 0
    tk = tqdm(data_loader, total=len(data_loader))
    for data in tk:
        for k, v in data.items():
            data[k] = v.to(config.DEVICE)
        optimizer.zero_grad()
        _, loss = model(**data)
        loss.backward()
        optimizer.step()
        fin_loss += loss.item()
    return fin_loss / len(data_loader)


def eval_fn(model, data_loader):
    model.eval()

    with torch.no_grad():
        fin_loss = 0
        fin_prediction = []

        tk = tqdm(data_loader, total=len(data_loader))
        for data in tk:
            for k, v in data.items():
                data[k] = v.to(config.DEVICE)
            batch_prediction, loss = model(**data)

            fin_loss += loss.item()
            fin_prediction.append(batch_prediction)  # Need to be changed for CPU

    return fin_prediction, fin_loss / len(data_loader)
