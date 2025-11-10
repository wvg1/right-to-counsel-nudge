
import os
import base64
from openai import AzureOpenAI
from dotenv import load_dotenv

load_dotenv()

endpoint = os.getenv("ENDPOINT_URL")
deployment = os.getenv("DEPLOYMENT_NAME")
subscription_key = os.getenv("AZURE_OPENAI_API_KEY")

if not endpoint or not deployment or not subscription_key:
    raise SystemExit("Missing Azure OpenAI credentials in .env file")

def truncate_text(text, max_chars=200000):
    """
    This function checks if text is too long and truncates if so.
    
    text: the string to check
    max_chars: maximum allowed characters
    
    """
    if len(text) <= max_chars:
        
        return text, False
    
    truncated = text[:max_chars] 
    
    # Try to cut at a sentence end (find last period)
    last_period = truncated.rfind('.') 
    
    if last_period > max_chars * 0.95:  
        truncated = truncated[:last_period + 1]  
    
    return truncated, True  

def data_from_llm(system_prompt, full_text):
    
    #truncate if needed
    processed_text, was_truncated = truncate_text(full_text, max_chars=200000)

    if was_truncated:
        print(f"Warning: Truncated from {len(full_text):,} to {len(processed_text):,} chars")

# Initialize Azure OpenAI client with key-based authentication
    client = AzureOpenAI(
        azure_endpoint=endpoint,
        api_key=subscription_key,
        api_version="2025-01-01-preview",
    )

    # IMAGE_PATH = "YOUR_IMAGE_PATH"
    # encoded_image = base64.b64encode(open(IMAGE_PATH, 'rb').read()).decode('ascii')

    # Prepare the chat prompt
    chat_prompt = [
        {
            "role": "system",
            "content": [
                {
                    "type": "text",
                    "text": system_prompt}
            ]
        },
        {
            "role": "user",
            "content": [
                {
                    "type": "text",
                    "text": processed_text}
            ]
        }
        
        
    ]

    # Include speech result if speech is enabled
    messages = chat_prompt

    # Generate the completion
    completion = client.chat.completions.create(
        model=deployment,
        messages=messages,
        max_tokens=2000,
        temperature=0.7,
        top_p=0.95,
        frequency_penalty=0,
        presence_penalty=0,
        stop=None,
        stream=False, 
        logprobs = True
    )
    result = completion.to_json()
    return(result)
        