# utility native libraries
import glob
import json
import pandas as pd
import pprint
from datetime import datetime

# installed libraries
import torch
import transformers
from transformers import AutoTokenizer, AutoModelForCausalLM, BitsAndBytesConfig
from transformers import AutoModelForSeq2SeqLM, AutoTokenizer
# quantization libraries
from peft import prepare_model_for_kbit_training
from peft import LoraConfig, get_peft_model
from sklearn.model_selection import KFold
from tqdm.notebook import tqdm



class DownloadModel:

    def _quantize_model(self, model_artifact: dict) -> dict:
        """
        A function to quantize the model using PEFT and LoRA
        """
        model = model_artifact["model"]
        config = LoraConfig(
            r=32,
            lora_alpha=64,
            target_modules=[
                "q_proj",
                "k_proj",
                "v_proj",
                "o_proj",
                "gate_proj",
                "up_proj",
                "down_proj",
                "lm_head",
            ],
            bias="none",
            lora_dropout=0.05,
            task_type="CAUSAL_LM",
        )

        model = get_peft_model(model, config)
        model_artifact["model"] = model

        return model_artifact


    def _get_mistral_model(self):
        """
        A function to download the mistral model from
        """
        model_id = "mistralai/Mistral-7B-Instruct-v0.2"
        tokenizer = AutoTokenizer.from_pretrained(model_id)

        bnb_config = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_use_double_quant=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_compute_dtype=torch.bfloat16
        )
        model = AutoModelForCausalLM.from_pretrained(
            model_id, quantization_config=bnb_config)
        model.gradient_checkpointing_enable()
        model = prepare_model_for_kbit_training(model)
        model_artifact = {"model": model, "tokenizer": tokenizer}
        model_artifact = self._quantize_model(model_artifact)
        return model_artifact

    def _get_llama_model(self):
        """
        A function to download the llama model from
        """
        model_id = "meta-llama/Meta-Llama-3.1-8B-Instruct"
        tokenizer = AutoTokenizer.from_pretrained(model_id)

        bnb_config = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_use_double_quant=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_compute_dtype=torch.bfloat16
        )
        model = AutoModelForSeq2SeqLM.from_pretrained(
            model_id, quantization_config=bnb_config)
        model_artifact = {"model": model, "tokenizer": tokenizer}
        model_artifact = self._quantize_model(model_artifact)
        return model_artifact


    def _get_eluether_model(self):
        """
        A function to download the eluether model from
        """
        model_id = "EleutherAI/pythia-70m"
        tokenizer = AutoTokenizer.from_pretrained(model_id)
        model = AutoModelForCausalLM.from_pretrained(model_id)
        model_artifact = {"model": model, "tokenizer": tokenizer}
        return model_artifact


    def _get_hermes_model(self):
        """
        A function to download the eluether model from
        """
        model_id = "NousResearch/Hermes-3-Llama-3.1-8B"
        tokenizer = AutoTokenizer.from_pretrained(model_id)

        bnb_config = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_use_double_quant=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_compute_dtype=torch.bfloat16
        )
        model = AutoModelForSeq2SeqLM.from_pretrained(
            model_id, quantization_config=bnb_config)
        model_artifact = {"model": model, "tokenizer": tokenizer}
        model_artifact = self._quantize_model(model_artifact)
        return model_artifact
    
    def download(self, model_name):
        """
        A function to interface the different model downloaders

        Args:
            model_name: str: name of the model to be downloaded
        
        Returns:
            the response of the model download function
        
        Raises:
            None
        """
        model_map = {
            "mistralai/Mistral-7B-Instruct-v0.2": self._get_mistral_model,
            "NousResearch/Hermes-3-Llama-3.1-8B": self._get_hermes_model,
            "EleutherAI/pythia-70m": self._get_eluether_model,
            "meta-llama/Meta-Llama-3.1-8B-Instruct": self._get_llama_model
        }
        if model_name not in model_map:
            raise ValueError("Model not found")
        
        return model_map[model_name]()



class PromptTemplates:
    def _input_output_template(self):
        pass

    def _icl_template(self):
        """
        A function to create the instruct response template

        Returns:
        dict: with response: 
            : without response:

        Raises:
        None
        """
        text_template_with_response = """
            Below is an instruction that describes a task, paired with an input that provides further context. Write a response that appropriately completes the request.


            ### Instruction:
            1. Understand the clinical text
            2. Extract the medicine prescription names
            3. Extract the dosage of medicine prescriptions
            4. Extract the seizure frequency
            5. The parts of sentences in <emphasize> tags are important
            5. structure  the response in the json format as mentioned in the example
            response

            ### Input:
            {input}

            ### Response:
            {response}
        """

        text_template_without_response = """
            Given the instruction, please write the response in the json format

            ### Instruction:
            Given the clinical text, please
            1. Extract the medicine prescription names
            2. Extract the dosage of medicine prescriptions
            3. Extract the seizure frequency
            4. structure the response in the json format


            ### Clinical Text:
            {clinical_text}

            ### Response:
        """
        with_response = []
        without_response = []

        questions = glob.glob("/content/modified_data/*.txt")
        responses = glob.glob("/content/modified_data/*.json")

        for clinical_text_file, response_file in zip(questions, responses):
            with open(clinical_text_file, "r") as f:
                clinical_text = f.read()

            with open(response_file, "r") as f:
                response = json.load(f)

            text_with_prompt_template_qa = text_template_with_response.format(
                clinical_text=clinical_text, response=response)
            with_response.append({
                "text": text_with_prompt_template_qa})

            text_with_prompt_template_q = text_template_without_response.format(
                clinical_text=clinical_text)
            without_response.append(
                {"test_input": text_with_prompt_template_q,
                "response": response})

        return {
            "with_response": with_response,
            "without_response": without_response
        }


class Preprocess:
    def _input_output_template_preprocess(self):
        pass

    def _icl_template_preprocess(self, tokenizer, text_data, text_type="with_response"):
        """
        A function to preprocess the dataset for training and evaluation

        Args:
            tokenizer: tokenizer to use
            text_data: text data to preprocess
            text_type: type of text data to preprocess

        Returns:
            train_dataset: tokenized train dataset
            eval_dataset: tokenized eval dataset

        Raises:
            None
        """
        data = text_data[text_type]

        if text_type == "with_response":
             # prepare the dataset
            tokenizer.padding_side = 'right'
            tokenizer.pad_token = tokenizer.eos_token

            tokenized_dataset = list(map(lambda x: tokenizer(x["text"]), data))
            return tokenized_dataset
        else:
            tokenized_dataset = list(map(lambda x: tokenizer(x["test_input"]), data))
            return tokenized_dataset
    
    def preprocess(self, template_type="icl", **kwargs):
        """
        A factory method function to interface the template associated preprocess function

        Args:
            template_type: str: type of the template
        
        Returns:
            data: str: 
        
        Raises:
            None
        """
        template_interfaces = {
            "icl": self._icl_template_preprocess,
            "instruct": self._input_output_template_preprocess
        }
        return template_interfaces[template_type](**kwargs)


class Postprocess:
    def _postprocess(self):
        pass


class Model:
    def train(self, model_name, model, tokenizer, tokenized_dataset, k_split=1):
        """
        A function to train the model

        Args:
            model_name: str: name of the model
            model: object: model object 
            data: pd.DataFrame: pandas dataframe containing the data
            k_split: int: number of splits to be made in the dataset
        
        Returns:
            finetuned_model: dict: model fine tuned on the data and the tokenizer
        
        Raises:
            None

        """
        # wandb setup
        project = "demo"
        run_name = project + model_name.split("/")[0].upper()
        output_dir = "./" + run_name

        kfold_splitter = KFold(n_splits=1, shuffle=True, random_state=42)

        # training arguments
        train_args = transformers.TrainingArguments(
                output_dir=output_dir,
                warmup_steps=5,
                per_device_train_batch_size=1,
                gradient_checkpointing=True,
                gradient_accumulation_steps=4,
                max_steps=100,
                learning_rate=2.5e-5, # Want about 10x smaller than the Mistral learning rate
                logging_steps=50,
                bf16=False,
                optim="paged_adamw_8bit",
                logging_dir="./logs",        # Directory for storing logs
                save_strategy="steps",       # Save the model checkpoint every logging step
                save_steps=50,                # Save checkpoints every 50 steps
                eval_strategy="steps", # Evaluate the model every logging step
                eval_steps=50,               # Evaluate and save checkpoints every 50 steps
                do_eval=True,                # Perform evaluation at the end of training
                report_to="wandb",           # Comment this out if you don't want to use weights & baises
                run_name=f"{run_name}-{datetime.now().strftime('%Y-%m-%d-%H-%M')}"          # Name of the W&B run (optional)
            )
        
        num_split = tqdm(total=k_split)
        for train_index, test_index in kfold_splitter.split(tokenized_dataset):
            num_split.update(1)
            train_dataset = [tokenized_dataset[i] for i in train_index]
            eval_dataset = [tokenized_dataset[i] for i in test_index]  
            
            if torch.cuda.device_count() > 1: # If more than 1 GPU
                model.is_parallelizable = True
                model.model_parallel = True
            
            trainer = transformers.Trainer(
                model=model,
                train_dataset=train_dataset,
                eval_dataset=eval_dataset,
                args=train_args,
                data_collator=transformers.DataCollatorForLanguageModeling(
                    tokenizer, mlm=False),
            )

            model.config.use_cache = False  
            
            trainer.train()

        model.save_pretrained(output_dir)
        num_split.close()
        return model


    def evaluate(self):
        """
        A function that implements the evaluation strategy for model
        """
        pass

    def inference(self, data, model, tokenizer, max_input_tokens=1000, max_output_tokens=250):
        """
        A function to perform model inferencing

        Args:
            data: str: data sample to infer on
            model: object: fine tuned model to perform the model inferencing on
            tokenizer: object: tokenizer to encode the sample text
            max_input_tokens: int: number of input tokens to consider
            max_output_tokens: int: number of the output tokens to consider
        
        Returns:
            generated_text_answer: str: text generated from the model
        
        Raises:
            None
        """
        # Tokenize
        input_ids = tokenizer.encode(
                data,
                return_tensors="pt",
                truncation=True,
                max_length=max_input_tokens
        )

        # Generate
        device = model.device
        generated_tokens_with_prompt = model.generate(
            input_ids=input_ids.to(device),
            max_length=max_output_tokens
        )

        # Decode
        generated_text_with_prompt = tokenizer.batch_decode(
            generated_tokens_with_prompt, 
            skip_special_tokens=True
        )

        # Strip the prompt
        generated_text_answer = generated_text_with_prompt[0][len(data):]

        return generated_text_answer

